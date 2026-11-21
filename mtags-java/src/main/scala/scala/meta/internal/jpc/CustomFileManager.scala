package scala.meta.internal.jpc

import java.io.BufferedInputStream
import java.io.InputStreamReader
import java.lang.{Iterable => JIterable}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.{util => ju}
import javax.tools.JavaFileManager
import javax.tools.JavaFileObject
import javax.tools.StandardLocation

import scala.jdk.CollectionConverters._
import scala.util.Using

import scala.meta.internal.metals.CtSym
import scala.meta.internal.mtags.SourcePath

/**
 * A module of the JDK, whose classes are read from an archive
 *
 * @param prefixes
 *   the archive entry prefixes the classes of the module are under: `classes/`
 *   in `.jmod` files, `<releases>/<module>/` in ct.sym (see [[CtSym]])
 * @param classSuffix
 *   the suffix of class entries: `.class` in `.jmod` files, `.sig` in ct.sym
 */
case class ModuleLocation(
    moduleName: String,
    path: Path,
    prefixes: Seq[String],
    classSuffix: String
) extends JavaFileManager.Location {
  override def getName(): String =
    toString
  override def isOutputLocation(): Boolean =
    false
}

object ModuleLocation {
  def jmod(jmod: Path): ModuleLocation =
    ModuleLocation(
      jmod.getFileName.toString.stripSuffix(".jmod"),
      jmod,
      Seq("classes/"),
      ".class"
    )
  def ctSym(ctSym: Path): Seq[ModuleLocation] =
    CtSym
      .layout(ctSym)
      .modules
      .toVector
      .sortBy(_._1)
      .map { case (module, prefixes) =>
        ModuleLocation(module, ctSym, prefixes, CtSym.classSuffix)
      }
}

/**
 * @param entryName
 *   path of the class file relative to its module, like
 *   `java/lang/String.class` (also for ct.sym signatures)
 * @param actualEntryName
 *   the name of the entry in the archive, like
 *   `classes/java/lang/String.class` or `MNO/java.base/java/lang/String.sig`
 */
case class MetalsJavaFileObject(
    name: String,
    lastModified: Long,
    path: Path,
    entryName: String,
    actualEntryName: String,
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

/**
 * A file manager reading the JDK system modules from `jdkClassFiles`, rather
 * than from the JDK javac runs on
 *
 * @param jdkClassFiles
 *   the `.jmod` files of the JDK, or its `lib/ct.sym` file (whose class
 *   signatures for the JDK's own release are used then)
 */
class CustomFileManager(
    jdkClassFiles: Seq[Path],
    underlying: javax.tools.JavaFileManager,
    ctx: SourcePath.Context
) extends ForwardingJavaFileManager(underlying) {

  /** Reads the `.jmod` files of `javaHome` */
  def this(
      javaHome: Path,
      underlying: javax.tools.JavaFileManager,
      ctx: SourcePath.Context
  ) =
    this(CustomFileManager.jmodFiles(javaHome), underlying, ctx)

  @volatile private var modsCache: JIterable[ju.Set[JavaFileManager.Location]] =
    null

  private val moduleEntriesCache =
    new ju.concurrent.ConcurrentHashMap[ModuleLocation, Vector[
      CustomFileManager.ModuleEntry
    ]]

  def resetCache(): Unit = {
    modsCache = null
    moduleEntriesCache.clear()
  }

  private def systemModules: JIterable[ju.Set[JavaFileManager.Location]] = {
    val valueOrNull = modsCache
    if (valueOrNull == null) {
      val mods = jdkClassFiles
        .filter(Files.isRegularFile(_))
        .flatMap { f =>
          if (CtSym.isCtSym(f)) ModuleLocation.ctSym(f)
          else if (f.getFileName.toString.endsWith(".jmod"))
            Seq(ModuleLocation.jmod(f))
          else Nil
        }
        .map(loc => loc: JavaFileManager.Location)
        .toSet
      val mods0 = List(mods.asJava).asJava
      modsCache = mods0
      mods0
    } else
      valueOrNull
  }

  private def moduleEntries(
      modLoc: ModuleLocation
  ): Vector[CustomFileManager.ModuleEntry] = {
    val valueOrNull = moduleEntriesCache.get(modLoc)
    if (valueOrNull == null) {
      val entries = ctx
        .entries(modLoc.path)
        .flatMap { ent =>
          val name = ent.pathInZip
          if (name.endsWith(modLoc.classSuffix))
            modLoc.prefixes.collectFirst {
              case prefix if name.startsWith(prefix) =>
                CustomFileManager.ModuleEntry(
                  name
                    .substring(prefix.length)
                    .stripSuffix(modLoc.classSuffix) + ".class",
                  name,
                  ent.lastModified
                )
            }
          else None
        }
        .toVector
      val previousOrNull = moduleEntriesCache.putIfAbsent(modLoc, entries)
      if (previousOrNull == null) entries
      else previousOrNull
    } else
      valueOrNull
  }

  private def fileObject(
      modLoc: ModuleLocation,
      entry: CustomFileManager.ModuleEntry
  ): MetalsJavaFileObject =
    MetalsJavaFileObject(
      entry.relPath,
      entry.lastModified,
      modLoc.path,
      entry.relPath,
      entry.entryName,
      ctx
    )

  override def listLocationsForModules(
      location: JavaFileManager.Location
  ): JIterable[ju.Set[JavaFileManager.Location]] =
    location match {
      case StandardLocation.SYSTEM_MODULES =>
        systemModules
      case _ =>
        super.listLocationsForModules(location)
    }

  override def getJavaFileForInput(
      location: JavaFileManager.Location,
      className: String,
      kind: JavaFileObject.Kind
  ): JavaFileObject =
    (location, kind) match {
      case (modLoc: ModuleLocation, JavaFileObject.Kind.CLASS) =>
        val relPath = className.replace('.', '/') + ".class"
        moduleEntries(modLoc)
          .find(_.relPath == relPath)
          .map(fileObject(modLoc, _))
          .orNull
      case _ =>
        super.getJavaFileForInput(location, className, kind)
    }

  override def getLocationForModule(
      location: JavaFileManager.Location,
      moduleName: String
  ): JavaFileManager.Location =
    location match {
      case StandardLocation.SYSTEM_MODULES =>
        listLocationsForModules(location).asScala
          .flatMap(_.asScala)
          .collectFirst {
            case modLoc: ModuleLocation if modLoc.moduleName == moduleName =>
              modLoc
          }
          .orNull
      case _ =>
        super.getLocationForModule(location, moduleName)
    }

  override def list(
      location: JavaFileManager.Location,
      packageName: String,
      kinds: ju.Set[JavaFileObject.Kind],
      recurse: Boolean
  ): JIterable[JavaFileObject] =
    location match {
      case modLoc: ModuleLocation =>
        val prefix =
          if (packageName.isEmpty) ""
          else packageName.replace('.', '/') + "/"
        moduleEntries(modLoc)
          .filter(ent => ent.relPath.startsWith(prefix))
          .filter(ent =>
            !ent.relPath.endsWith(
              "/module-info.class"
            ) && ent.relPath != "module-info.class"
          )
          .filter(ent =>
            recurse || !ent.relPath.substring(prefix.length).contains('/')
          )
          .map(fileObject(modLoc, _): javax.tools.JavaFileObject)
          .toList
          .asJava
      case _ =>
        super.list(location, packageName, kinds, recurse)
    }

  override def inferModuleName(location: JavaFileManager.Location): String =
    location match {
      case modLoc: ModuleLocation =>
        modLoc.moduleName
      case _ =>
        super.inferModuleName(location)
    }

  override def inferBinaryName(
      location: JavaFileManager.Location,
      fileObj: JavaFileObject
  ): String =
    (location, fileObj) match {
      case (_: ModuleLocation, obj: MetalsJavaFileObject)
          if obj.entryName.endsWith(".class") =>
        obj.entryName.stripSuffix(".class").split('/').mkString(".")
      case _ =>
        super.inferBinaryName(location, fileObj)
    }

}

object CustomFileManager {

  /**
   * @param relPath
   *   path of the class relative to its module, like `java/lang/String.class`
   * @param entryName
   *   name of the archive entry
   */
  final case class ModuleEntry(
      relPath: String,
      entryName: String,
      lastModified: Long
  )

  def jmodFiles(javaHome: Path): Seq[Path] = {
    val jmodsDir = javaHome.resolve("jmods")
    if (Files.isDirectory(jmodsDir))
      Using.resource(Files.list(jmodsDir)) { list =>
        list
          .iterator()
          .asScala
          .filter(_.toString.endsWith(".jmod"))
          .filter(Files.isRegularFile(_))
          .toVector
      }
    else
      Nil
  }

  /** Whether a class path entry is one of the JDK files the file manager reads */
  def isJdkClassFile(path: Path): Boolean =
    CtSym.isCtSym(path) || path.getFileName.toString.endsWith(".jmod")
}
