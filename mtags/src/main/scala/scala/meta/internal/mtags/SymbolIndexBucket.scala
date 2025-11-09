package scala.meta.internal.mtags

import java.nio.CharBuffer
import java.util.logging.Level
import java.util.logging.Logger

import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

import scala.meta.Dialect
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PlatformFileIO
import scala.meta.internal.metals.JdkVersion0
import scala.meta.internal.mtags.ScalametaCommonEnrichments._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.AbsolutePath
import java.nio.file.Path
import java.nio.file.Files
import java.util.zip.ZipFile
import scala.meta.inputs.Input
import java.nio.charset.StandardCharsets
import java.nio.charset.Charset
import java.net.URI
import java.nio.file.Paths
import java.util.concurrent.ConcurrentHashMap
import java.io.FileNotFoundException
import scala.meta.pc.reports.ReportContext

final case class SymbolLocation(
    path: SourcePath,
    range: Option[s.Range]
)

sealed abstract class SourcePath extends Product with Serializable {
  def uri: String
  def filePath: Option[Path]
  def content(charSet: Charset = StandardCharsets.UTF_8)(implicit
      context: SourcePath.Context
  ): String
  def toInput(implicit context: SourcePath.Context): Input.VirtualFile =
    Input.VirtualFile(uri, content())

  def exists()(implicit context: SourcePath.Context): Boolean

  def isScalaScript: Boolean = false
  def isMill: Boolean = false

  def extension: String
}

object SourcePath {
  def apply(uri: String): SourcePath = {
    val uri0 = new URI(uri)
    if (uri0.getScheme == "jar" && uri0.getRawSchemeSpecificPart != null)
      uri0.getRawSchemeSpecificPart.split("!/", 2) match {
        case Array(zipUri, pathInZip) =>
          ZipEntry(Paths.get(new URI(zipUri)), pathInZip)
        case Array(_) =>
          throw new Exception(
            s"Malformed jar URI: '$uri' (missing '!' path-in-zip part, like in jar:file://path/to.zip!path/in/zip)"
          )
      }
    else
      Standard(Paths.get(uri0))
  }

  final case class Standard(path: Path) extends SourcePath {
    def uri: String = path.toUri.toASCIIString
    def filePath: Option[Path] = Some(path)
    def content(charSet: Charset)(implicit
        context: SourcePath.Context
    ): String =
      FileIO.slurp(AbsolutePath(path), charSet)
    def exists()(implicit context: SourcePath.Context): Boolean =
      Files.exists(path)
    override def isScalaScript: Boolean =
      path.toString.isScalaScript
    override def isMill: Boolean =
      path.toString.isMill
    def extension: String =
      AbsolutePath(path).extension
  }
  final case class ZipEntry(zipPath: Path, pathInZip: String)
      extends SourcePath {
    def uri: String =
      "jar:" + zipPath.toUri.toASCIIString + "!/" + pathInZip
    def filePath: Option[Path] = None
    def content(
        charSet: Charset
    )(implicit context: SourcePath.Context): String = {
      val zf = context.get(zipPath)
      val ent = zf.getEntry(pathInZip)
      if (ent == null)
        throw new FileNotFoundException(uri)
      new String(zf.getInputStream(ent).readAllBytes(), charSet)
    }
    def exists()(implicit context: SourcePath.Context): Boolean =
      Files.exists(zipPath) && context.get(zipPath).getEntry(pathInZip) != null
    def extension: String =
      pathInZip.split('/').last.split('.') match {
        case Array(_) => ""
        case other => other.last
      }
  }

  final class Context extends AutoCloseable { ctx =>
    private val map = new ConcurrentHashMap[Path, ZipFile]
    def get(path: Path): ZipFile =
      Option(map.get(path)) match {
        case Some(zf) => zf
        case None =>
          val zf = new ZipFile(path.toFile)
          val previousOpt = Option(map.putIfAbsent(path, zf))
          previousOpt match {
            case Some(previous) =>
              zf.close()
              previous
            case None =>
              zf
          }
      }
    def entries(path: Path): Iterator[ZipEntry] =
      get(path)
        .entries()
        .asScala
        .filter(!_.getName.endsWith("/"))
        .map(ent => ZipEntry(path, ent.getName))
    def close(): Unit =
      for ((path, zf) <- map.asScala.toVector) {
        map.remove(path, zf)
        zf.close()
      }

    lazy val iface: scala.meta.pc.SourcePathContext =
      new scala.meta.pc.SourcePathContext {
        def get(path: Path) = ctx.get(path)
        def entries(path: Path) =
          ctx
            .entries(path)
            .map(ent =>
              new java.util.AbstractMap.SimpleEntry(
                ent.zipPath,
                ent.pathInZip
              ): java.util.Map.Entry[Path, String]
            )
            .asJava
        def actualContext(): Object = ctx
      }
  }

  object Context {
    def from(iface: scala.meta.pc.SourcePathContext): Context =
      iface.actualContext().asInstanceOf[Context]
  }

  def withContext[T](f: Context => T): T = {
    var context: Context = null
    try {
      context = new Context
      f(context)
    } finally {
      if (context != null)
        context.close()
    }
  }
}

/**
 * Index split on buckets per dialect in order to have a constant time
 * and low memory footprint to infer dialect for SymbolDefinition because
 * it's used in WorkspaceSymbolProvider
 *
 * @param toplevels keys are non-trivial toplevel symbols and values are the file
 *                  the symbols are defined in.
 * @param definitions keys are global symbols and the values are the files the symbols
 *                    are defined in. Difference between toplevels and definitions
 *                    is that toplevels contains only symbols generated by ScalaToplevelMtags
 *                    while definitions contains only symbols generated by ScalaMtags.
 */
class SymbolIndexBucket(
    toplevels: AtomicTrieMap[String, Set[SourcePath]],
    definitions: AtomicTrieMap[String, Set[SymbolLocation]],
    sourceJars: OpenClassLoader,
    toIndexSource: AbsolutePath => AbsolutePath = identity,
    mtags: Mtags,
    dialect: Dialect,
    onError: PartialFunction[Throwable, Unit],
    javaHome: Path
) {

  private val logger = Logger.getLogger(classOf[SymbolIndexBucket].getName)

  def close(): Unit = () // sourceJars.close()

  def addSourceDirectory(
      dir: AbsolutePath
  )(implicit rc: ReportContext): List[IndexingResult] = {
    if (sourceJars.addEntry(dir.toNIO)) {
      dir.listRecursive.toList.flatMap {
        case source if source.isScala =>
          addSourceFile(source.toInput, isJava = false)
        case source if source.isJava =>
          addJavaSourceFile(source.toInput) match {
            case Nil => None
            case topLevels =>
              Some(
                IndexingResult(
                  SourcePath.Standard(source.toNIO),
                  topLevels,
                  overrides = Nil,
                  toplevelMembers = Nil
                )
              )
          }
        case _ =>
          None
      }
    } else List.empty
  }

  def addSourceJar(
      jar: AbsolutePath,
      reindex: Boolean = false,
      isPureJava: Boolean = false
  )(implicit
      ctx: SourcePath.Context,
      rc: ReportContext
  ): List[IndexingResult] =
    if (reindex || sourceJars.addEntry(jar.toNIO))
      ctx
        .entries(jar.toNIO)
        .flatMap { source =>
          if (source.uri.endsWith(".scala") && !isPureJava)
            addSourceFile(source.toInput, isJava = false)
          else if (source.uri.endsWith(".java"))
            // addSourceFile(source.toInput, isJava = true) // ?
            addJavaSourceFile(source.toInput) match {
              case Nil => None
              case topLevels =>
                Some(
                  IndexingResult(
                    source,
                    topLevels,
                    overrides = Nil,
                    toplevelMembers = Nil
                  )
                )
            }
          else
            None
        }
        .toList
    else
      List.empty

  def indexSourceJar(jar: AbsolutePath, isJava: Boolean)(implicit
      ctx: SourcePath.Context
  ): List[IndexingResult] =
    ctx
      .entries(jar.toNIO)
      .filter(_.pathInZip.endsWith(".scala"))
      .map(source => indexSource(source.toInput, isJava = isJava))
      .toList

  def addIndexedSourceJar(
      jar: AbsolutePath,
      symbols: List[(String, SourcePath)]
  ): Unit = {
    if (sourceJars.addEntry(jar.toNIO)) {
      symbols.foreach { case (sym, path) =>
        toplevels.updateWith(sym) {
          case Some(acc) => Some(acc + path)
          case None => Some(Set(path))
        }
      }
    }
    PlatformFileIO.newJarFileSystem(jar, create = false)
  }

  /* Sometimes source jars have additional nested directories,
   * in that case java toplevel is not "trivial".
   * See: https://github.com/scalameta/metals/issues/3815
   */
  def addJavaSourceFile(
      input: Input.VirtualFile
  )(implicit rc: ReportContext): List[String] = {
    new JavaToplevelMtags(
      input,
      includeInnerClasses = false
    ).readPackage match {
      case Nil => Nil
      case packageParts =>
        val className = input.path.stripSuffix(".java")
        val symbol = packageParts.mkString("", "/", s"/$className#")
        val isTrivialToplevelSymbol0 = AbsolutePath(input.path)
          .toIdeallyRelativeURI()
          .exists { subPath =>
            isTrivialToplevelSymbol(
              subPath,
              symbol,
              extension = "java"
            )
          }
        if (isTrivialToplevelSymbol0) Nil
        else {
          toplevels.updateWith(symbol) {
            case Some(acc) => Some(acc + SourcePath(input.path))
            case None => Some(Set(SourcePath(input.path)))
          }
          List(symbol)
        }
    }
  }

  def addSourceFile(
      input: Input.VirtualFile,
      isJava: Boolean
  ): Option[IndexingResult] = try {
    val IndexingResult(path, topLevels, overrides, toplevelMembers) =
      indexSource(input, isJava)
    topLevels.foreach { symbol =>
      toplevels.updateWith(symbol) {
        case Some(acc) => Some(acc + SourcePath(input.path))
        case None => Some(Set(SourcePath(input.path)))
      }
    }
    Some(IndexingResult(path, topLevels, overrides, toplevelMembers))
  } catch {
    case NonFatal(e) =>
      onError(e)
      None
  }

  def indexSource(
      input: Input.VirtualFile,
      isJava: Boolean
  ): IndexingResult = {
    val source = SourcePath(input.path)
    val (doc, overrides, toplevelMembers) =
      mtags.extendedIndexing(input, dialect)
    val sourceTopLevels =
      doc.occurrences.iterator
        .filterNot(_.symbol.isPackage)
        .map(_.symbol)
    val topLevels =
      if (input.path.isScalaScript) sourceTopLevels.toList
      else if (isJava) {
        sourceTopLevels.toList.headOption
          .filter(sym => !isTrivialToplevelSymbol(input.path, sym, "java"))
          .toList
      } else
        AbsolutePath(input.path).toIdeallyRelativeURI() match {
          case Some(subPath) =>
            sourceTopLevels
              .filter(sym => !isTrivialToplevelSymbol(subPath, sym, "scala"))
              .toList
          case None =>
            sourceTopLevels.toList
        }
    IndexingResult(source, topLevels, overrides, toplevelMembers)
  }

  // Returns true if symbol is com/foo/Bar# and path is /com/foo/Bar.scala
  // Such symbols are "trivial" because their definition location can be computed
  // on the fly.
  private def isTrivialToplevelSymbol(
      path: String,
      symbol: String,
      extension: String = "scala"
  ): Boolean = {
    val pathBuffer =
      CharBuffer.wrap(path).subSequence(1, path.length - extension.length - 1)
    val symbolBuffer =
      CharBuffer.wrap(symbol).subSequence(0, symbol.length - 1)
    pathBuffer.equals(symbolBuffer)
  }

  def addToplevelSymbol(
      path: String,
      source: SourcePath,
      toplevel: String
  ): Unit = {
    if (source.isScalaScript || !isTrivialToplevelSymbol(path, toplevel)) {
      toplevels.updateWith(toplevel) {
        case Some(acc) => Some(acc + source)
        case None => Some(Set(source))
      }
    }
  }

  def findFileForToplevel(
      topLevelSymbol: Symbol
  )(implicit ctx: SourcePath.Context): List[(SourcePath, Dialect)] = {
    toplevels
      .get(topLevelSymbol.toString())
      .map(_.toList)
      .orElse(loadFromSourceJars(trivialPaths(topLevelSymbol)))
      .orElse(loadFromSourceJars(modulePaths(topLevelSymbol)))
      .getOrElse(Nil)
      .map(x => (x, dialect))
  }

  def query(symbol: Symbol): List[SymbolDefinition] =
    SourcePath.withContext { implicit ctx =>
      query0(symbol, symbol)
    }

  /**
   * Returns the file where symbol is defined, if any.
   *
   * Uses two strategies to recover from missing symbol definitions:
   * - try to enter the toplevel symbol definition, then lookup symbol again.
   * - if the symbol is synthetic, for examples from a case class of macro annotation,
   *  fall back to related symbols from the enclosing class, see `DefinitionAlternatives`.
   *
   * @param querySymbol The original symbol that was queried by the user.
   * @param symbol The symbol that
   * @return
   */
  private def query0(
      querySymbol: Symbol,
      symbol: Symbol
  )(implicit ctx: SourcePath.Context): List[SymbolDefinition] = {

    removeOldEntries(symbol)

    if (!definitions.contains(symbol.value)) {
      // Fallback 1: enter the toplevel symbol definition
      val toplevel = symbol.toplevel
      val files = toplevels.get(toplevel.value)
      files match {
        case Some(files) =>
          files.map(_.toInput).foreach(addMtagsSourceFile(_))
        case _ =>
          loadFromSourceJars(trivialPaths(toplevel))
            .orElse(loadFromSourceJars(modulePaths(toplevel)))
            .foreach(_.foreach(p => addMtagsSourceFile(p.toInput)))
      }
      if (!definitions.contains(symbol.value)) {
        // Fallback 2: try with files for companion class
        if (toplevel.value.endsWith(".")) {
          val toplevelAlternative = s"${toplevel.value.stripSuffix(".")}#"
          for {
            companionClassFile <- toplevels
              .get(toplevelAlternative)
              .toSet
              .flatten
            if !files.exists(_.contains(companionClassFile))
          } addMtagsSourceFile(companionClassFile.toInput)
        }
      }
    }
    if (!definitions.contains(symbol.value)) {
      // Fallback 3: guess related symbols from the enclosing class.
      DefinitionAlternatives(symbol).flatMap(query0(querySymbol, _))
    } else {
      definitions
        .get(symbol.value)
        .map { paths =>
          paths.map { location =>
            SymbolDefinition(
              querySymbol = querySymbol,
              definitionSymbol = symbol,
              path = location.path,
              dialect = dialect,
              range = location.range,
              kind = None,
              properties = 0
            )
          }.toList
        }
        .getOrElse(List.empty)
    }
  }

  /**
   * Remove possible old, outdated entries from the toplevels and definitions.
   * This action is performed when a symbol is queried, to avoid returning incorrect results.
   */
  private def removeOldEntries(
      symbol: Symbol
  )(implicit ctx: SourcePath.Context): Unit = {
    val exists =
      (toplevels.get(symbol.value).getOrElse(Set.empty) ++ definitions
        .get(symbol.value)
        .map(_.map(_.path))
        .getOrElse(Set.empty)).filter(_.exists())

    toplevels.updateWith(symbol.value) {
      case None => None
      case Some(acc) =>
        val updated = acc.filter(exists(_))
        if (updated.isEmpty) None
        else Some(updated)
    }

    definitions.updateWith(symbol.value) {
      case None => None
      case Some(acc) =>
        val updated = acc.filter(loc => exists(loc.path))
        if (updated.isEmpty) None
        else Some(updated)
    }
  }

  private def toIndexInput(
      input: Input.VirtualFile
  )(implicit ctx: SourcePath.Context): Input.VirtualFile =
    SourcePath(input.path) match {
      case s: SourcePath.Standard =>
        val toIndex = toIndexSource(AbsolutePath(s.path))
        if (toIndex.toNIO == s.path) input
        else
          s.copy(path = toIndex.toNIO).toInput
      case _: SourcePath.ZipEntry =>
        input
    }

  private def allSymbols(
      input: Input.VirtualFile
  )(implicit ctx: SourcePath.Context): s.TextDocument = {
    val toIndexInput0 = toIndexInput(input)
    mtags.allSymbols(toIndexInput0, dialect)
  }

  private def extension(filename: String): String = {
    val idx = filename.lastIndexOf('.')
    if (idx == -1) ""
    else filename.substring(idx + 1)
  }

  // similar as addSourceFile except indexes all global symbols instead of
  // only non-trivial toplevel symbols.
  private def addMtagsSourceFile(
      input: Input.VirtualFile,
      retry: Boolean = true
  )(implicit ctx: SourcePath.Context): Unit = try {
    val docs: s.TextDocuments = extension(input.path) match {
      case "scala" | "java" | "sc" =>
        val document = allSymbols(input)
        s.TextDocuments(List(document))
      case _ =>
        s.TextDocuments(Nil)
    }
    if (docs.documents.nonEmpty) {
      addTextDocuments(SourcePath(input.path), docs)
    }
  } catch {
    case NonFatal(e) =>
      logger.log(Level.WARNING, s"Error indexing ${input.path}", e)
      if (retry) addMtagsSourceFile(input, retry = false)
  }

  // Records all global symbol definitions.
  private def addTextDocuments(
      path: SourcePath,
      docs: s.TextDocuments
  ): Unit = {
    docs.documents.foreach { document =>
      document.occurrences.foreach { occ =>
        if (occ.symbol.isGlobal && occ.role.isDefinition) {
          definitions.updateWith(occ.symbol) {
            case Some(acc) => Some(acc + SymbolLocation(path, occ.range))
            case None => Some(Set(SymbolLocation(path, occ.range)))
          }
        } else {
          // do nothing, we only care about global symbol definitions.
        }
      }
    }
  }

  // Returns the first path that resolves to a file.
  private def loadFromSourceJars(
      paths: List[String]
  )(implicit ctx: SourcePath.Context): Option[List[SourcePath]] = {
    paths match {
      case Nil => None
      case head :: tail =>
        val files = sourceJars
          .list()
          .iterator
          .map(jar => SourcePath.ZipEntry(jar, head))
          .filter(ent => ent.exists())
          .toList
        if (files.isEmpty) loadFromSourceJars(tail)
        else Some(files)
    }
  }

  // Returns relative file paths for trivial toplevel symbols, example:
  // Input:  scala/collection/immutable/List#
  // Output: scala/collection/immutable/List.scala
  //         scala/collection/immutable/List.java
  private def trivialPaths(toplevel: Symbol): List[String] = {
    val noExtension = toplevel.value.stripSuffix(".").stripSuffix("#")
    List(
      noExtension + ".scala",
      noExtension + ".java"
    )
  }

  private lazy val modules = {
    val srcZipCandidates =
      Seq(javaHome.resolve("src.zip"), javaHome.resolve("lib/src.zip"))
    val srcZip = srcZipCandidates.filter(Files.exists(_)).headOption.getOrElse {
      sys.error(s"src.zip not found among $srcZipCandidates")
    }
    val zf = new ZipFile(srcZip.toFile)
    try {
      import scala.jdk.CollectionConverters._
      zf.entries()
        .asScala
        .map(_.getName)
        .filter(_.endsWith("/module-info.java"))
        .map(_.stripSuffix("/module-info.java"))
        .filter(!_.contains("/"))
        .toList
    } finally {
      zf.close()
    }
  }

  private lazy val javaVer =
    JdkVersion0.fromJavaHome(javaHome).getOrElse {
      sys.error(s"Cannot get Java version of $javaHome")
    }
  private def modulePaths(toplevel: Symbol): List[String] =
    if (javaVer.major >= 9) {
      val noExtension = toplevel.value.stripSuffix(".").stripSuffix("#")
      modules.flatMap { module =>
        List(
          s"$module/$noExtension.java",
          s"$module/$noExtension.scala"
        )
      }
    } else
      Nil
}

object SymbolIndexBucket {

  def empty(
      dialect: Dialect,
      mtags: Mtags,
      sourceJars: OpenClassLoader,
      toIndexSource: AbsolutePath => AbsolutePath,
      onError: PartialFunction[Throwable, Unit],
      javaHome: Path
  ): SymbolIndexBucket =
    new SymbolIndexBucket(
      AtomicTrieMap.empty,
      AtomicTrieMap.empty,
      sourceJars,
      toIndexSource,
      mtags,
      dialect,
      onError,
      javaHome
    )

}
