package scala.meta.internal.pc

import java.io.File
import java.io.Writer
import java.net.URI
import java.nio.file.Path
import javax.tools.Diagnostic
import javax.tools.DiagnosticListener
import javax.tools.JavaCompiler
import javax.tools.JavaFileManager
import javax.tools.JavaFileObject
import javax.tools.ToolProvider

import scala.jdk.CollectionConverters._

import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.SymbolSearch

import com.sun.source.util.JavacTask
import com.sun.source.util.TreePath
import java.{util => ju}
import java.lang.{Iterable => JIterable}
import javax.tools.StandardLocation
import java.nio.file.Files
import scala.util.Using
import java.io.InputStreamReader
import java.nio.charset.StandardCharsets
import java.io.BufferedInputStream
import scala.meta.internal.mtags.SourcePath

class JavaMetalsGlobal(
    javaFileManager: JavaFileManager,
    val search: SymbolSearch,
    val metalsConfig: PresentationCompilerConfig,
    val classpath: Seq[Path]
) {
  var lastVisitedParentTrees: List[TreePath] = Nil

  def compilerTreeNode(
      scanner: JavaTreeScanner,
      position: CursorPosition
  ): Option[TreePath] = {
    scanner.scan(scanner.root, position)
    lastVisitedParentTrees = scanner.lastVisitedParentTrees
    lastVisitedParentTrees.headOption
  }

  def compilationTask(sourceCode: String, uri: URI): JavacTask = {
    val javaFileObject = SourceJavaFileObject.make(sourceCode, uri)
    // kind of meh, but does the job
    val (bootClassPath, userClassPath) =
      classpath.partition(_.getFileName.toString == "rt.jar")
    val javaVersion = if (bootClassPath.isEmpty) "17" else "1.8"

    val javaVersionOptions =
      List("--source", javaVersion, "--target", javaVersion)
    val classPathOptions =
      if (bootClassPath.isEmpty)
        List("-classpath", classpath.mkString(File.pathSeparator))
      else
        List(
          s"-bootclasspath",
          s"${bootClassPath.mkString(File.pathSeparator)}",
          "-classpath",
          userClassPath.mkString(File.pathSeparator)
        )
    JavaMetalsGlobal.classpathCompilationTask(
      javaFileObject,
      None,
      javaVersionOptions ++ classPathOptions,
      javaFileManager
    )
  }

}

case class ModuleLocation(
    moduleName: String,
    path: Path
) extends JavaFileManager.Location {
  override def getName(): String =
    toString
  override def isOutputLocation(): Boolean =
    false
}

case class MetalsJavaFileObject(
    name: String,
    lastModified: Long,
    path: Path,
    entryName: String,
    ctx: SourcePath.Context
) extends javax.tools.JavaFileObject {
  def getKind = javax.tools.JavaFileObject.Kind.CLASS

  def isNameCompatible(
      simpleName: String,
      kind: javax.tools.JavaFileObject.Kind
  ): Boolean =
    // not too sure what we should do here
    kind == javax.tools.JavaFileObject.Kind.CLASS
  def getAccessLevel(): javax.lang.model.element.Modifier = null
  def getNestingKind(): javax.lang.model.element.NestingKind = null

  private def actualEntryName =
    s"classes/$entryName"

  def getName(): String = name
  def toUri(): java.net.URI =
    new URI(s"jar:${path.toUri.toASCIIString}!/$actualEntryName")

  def getLastModified(): Long = lastModified

  def openInputStream(): java.io.InputStream = {
    val zf = ctx.get(path)
    val ent = zf.getEntry(actualEntryName)
    new BufferedInputStream(zf.getInputStream(ent))
  }
  def openReader(x$1: Boolean): java.io.Reader =
    new InputStreamReader(openInputStream())
  def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = {
    // FIXME What about ignoreEncodingErrors?
    val bytes = openInputStream().readAllBytes()
    new String(bytes, StandardCharsets.UTF_8) // FIXME Charset?
  }

  def delete(): Boolean = ???
  def openWriter(): java.io.Writer = ???
  def openOutputStream(): java.io.OutputStream = ???
}

class CustomFileManager(
    javaHome: Path,
    underlying: javax.tools.JavaFileManager,
    ctx: SourcePath.Context
) extends ForwardingJavaFileManager(underlying) {

  private val modsCache = new ju.concurrent.ConcurrentHashMap[Path, JIterable[
    ju.Set[JavaFileManager.Location]
  ]]

  def resetCache(): Unit =
    modsCache.clear()

  override def listLocationsForModules(
      location: JavaFileManager.Location
  ): JIterable[ju.Set[JavaFileManager.Location]] = {
    val res = location match {
      case StandardLocation.SYSTEM_MODULES =>
        val valueOrNull = modsCache.get(javaHome)
        if (valueOrNull == null) {
          val jmodsDir = javaHome.resolve("jmods")
          val mods =
            if (Files.isDirectory(jmodsDir))
              Using.resource(Files.list(jmodsDir)) { list =>
                list
                  .iterator()
                  .asScala
                  .filter(_.toString.endsWith(".jmod"))
                  .filter(Files.isRegularFile(_))
                  .map(jmod =>
                    ModuleLocation(
                      jmod.getFileName.toString.stripSuffix(".jmod"),
                      jmod
                    ): JavaFileManager.Location
                  )
                  .toSet
              }
            else
              Set.empty[JavaFileManager.Location]
          val mods0 = List(mods.asJava).asJava
          val prevOrNull = modsCache.putIfAbsent(javaHome, mods0)
          if (prevOrNull == null) mods0
          else prevOrNull
        } else
          valueOrNull
      case _ =>
        super.listLocationsForModules(location)
    }
    // scribe.info(s"JavaFileManager.listLocationsForModules($location) = " + pprint.apply(res.asScala.map(_.asScala)))
    res
  }

  override def list(
      location: JavaFileManager.Location,
      packageName: String,
      kinds: ju.Set[JavaFileObject.Kind],
      recurse: Boolean
  ): JIterable[JavaFileObject] = {
    val res: JIterable[JavaFileObject] = location match {
      case modLoc: ModuleLocation =>
        ctx
          .entries(modLoc.path)
          .filter(ent => ent.pathInZip.startsWith("classes/"))
          .filter(ent => ent.pathInZip.endsWith(".class"))
          .map { ent =>
            MetalsJavaFileObject(
              ent.pathInZip.stripPrefix("classes/"),
              ent.lastModified,
              modLoc.path,
              ent.pathInZip.stripPrefix("classes/"),
              ctx
            ): javax.tools.JavaFileObject
          }
          .toList
          .asJava
      case _ =>
        val res0 = super.list(location, packageName, kinds, recurse)
        // scribe.info(s"JavaFileManager.list($location (${location.getClass}), $packageName, $kinds, $recurse) = " + pprint.apply(res0.asScala))
        res0
    }
    res
  }

  override def inferModuleName(location: JavaFileManager.Location): String = {
    val res = location match {
      case modLoc: ModuleLocation =>
        modLoc.moduleName
      case _ =>
        val res0 = super.inferModuleName(location)
        // scribe.info(s"JavaFileManager.inferModuleName($location) = " + pprint.apply(res0))
        res0
    }
    // scribe.info(s"inferModuleName($location) = " + pprint.apply(res))
    res
  }

  override def inferBinaryName(
      location: JavaFileManager.Location,
      fileObj: JavaFileObject
  ): String = {
    val res = (location, fileObj) match {
      case (_: ModuleLocation, obj: MetalsJavaFileObject)
          if obj.entryName.endsWith(".class") =>
        obj.entryName.stripSuffix(".class").split('/').mkString(".")
      case _ =>
        val res0 = super.inferBinaryName(location, fileObj)
        res0
    }
    // scribe.info(s"inferBinaryName($location, $fileObj) = " + pprint.apply(res))
    res
  }

}

object JavaMetalsGlobal {

  val COMPILER: JavaCompiler = ToolProvider.getSystemJavaCompiler()

  private val noopDiagnosticListener = new DiagnosticListener[JavaFileObject] {

    // ignore errors since presentation compiler will have a lot of transient ones
    override def report(diagnostic: Diagnostic[_ <: JavaFileObject]): Unit = ()
  }

  def makeFileObject(file: File): JavaFileObject = {
    val fileManager = COMPILER.getStandardFileManager(null, null, null)
    val files = fileManager.getJavaFileObjectsFromFiles(List(file).asJava)
    files.iterator().next()
  }

  def baseCompilationTask(
      javaFileManager: JavaFileManager,
      sourceCode: String,
      uri: URI
  ): JavacTask = {
    val javaFileObject = SourceJavaFileObject.make(sourceCode, uri)
    JavaMetalsGlobal.classpathCompilationTask(
      javaFileObject,
      None,
      Nil,
      javaFileManager
    )
  }
  def classpathCompilationTask(
      javaFileObject: JavaFileObject,
      out: Option[Writer],
      allOptions: List[String],
      fileManager: JavaFileManager
  ): JavacTask = {
    COMPILER
      .getTask(
        out.orNull,
        fileManager,
        noopDiagnosticListener,
        allOptions.asJava,
        null,
        List(javaFileObject).asJava
      )
      .asInstanceOf[JavacTask]
  }

  def scanner(task: JavacTask): JavaTreeScanner = {
    val elems = task.parse()
    task.analyze()
    val root = elems.iterator().next()

    new JavaTreeScanner(task, root)
  }
}
