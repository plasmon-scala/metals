package scala.meta.internal.metals

import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util
import java.util.jar.JarFile
import java.util.logging.Level
import java.util.logging.Logger

import scala.reflect.NameTransformer
import scala.util.control.NonFatal

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.mtags.CommonMtagsEnrichments._

/**
 * An index to lookup classfiles contained in a given classpath.
 */
class PackageIndex() {
  val logger: Logger = Logger.getLogger(classOf[PackageIndex].getName)
  val packages = new util.HashMap[String, util.Set[String]]()
  private val isVisited = new util.HashSet[Path]()
  private val enterPackage =
    new util.function.Function[String, util.HashSet[String]] {
      override def apply(t: String): util.HashSet[String] = {
        new util.HashSet[String]()
      }
    }
  def visit(entry: Path, isExcludedPackage: String => Boolean): Unit =
    if (!isVisited.contains(entry)) {
      isVisited.add(entry)
      try {
        if (Files.isDirectory(entry))
          visitDirectoryEntry(entry)
        else if (Files.isRegularFile(entry) && entry.toString.endsWith(".jar"))
          visitJarEntry(entry, isExcludedPackage = isExcludedPackage)
        else if (Files.isRegularFile(entry) && entry.toString.endsWith(".jmod"))
          visitJarEntry(
            entry,
            isMod = true,
            isExcludedPackage = isExcludedPackage
          )
      } catch {
        case NonFatal(e) =>
          logger.log(Level.SEVERE, entry.toURI.toString, e)
      }
    }

  def addMember(pkg: String, member: String): Unit = {
    if (!member.contains("module-info.class")) {
      val members = packages.computeIfAbsent(pkg, enterPackage)
      members.add(NameTransformer.decode(member))
    }
  }

  private def visitDirectoryEntry(dir: Path): Unit = {
    Files.walkFileTree(
      dir,
      new SimpleFileVisitor[Path] {
        override def visitFile(
            file: Path,
            attrs: BasicFileAttributes
        ): FileVisitResult = {
          val member = file.getFileName.toString
          Option(file.getParent)
            .filter(_ => member.endsWith(".class"))
            .foreach { parent =>
              val relpath = dir.relativize(parent)
              val pkg = relpath.toURI(isDirectory = true).toString
              addMember(pkg, member)
            }
          FileVisitResult.CONTINUE
        }
        override def preVisitDirectory(
            dir: Path,
            attrs: BasicFileAttributes
        ): FileVisitResult = {
          if (dir.endsWith("META-INF")) FileVisitResult.SKIP_SUBTREE
          else FileVisitResult.CONTINUE
        }
      }
    )
  }

  /**
   * Returns the parent directory of the absolute string path
   *
   * Examples:
   *
   * {{{
   *   dirname("/a/b/") == "/a/"
   *   dirname("/a/b") == "/a/"
   *   dirname("/") == "/"
   * }}}
   *
   * @param abspath
   *   a string path that matches the syntax of ZipFile entries.
   */
  def dirname(abspath: String): String = {
    val isDir = abspath.endsWith("/")
    val end =
      if (isDir) abspath.lastIndexOf('/', abspath.length - 2)
      else abspath.lastIndexOf('/')
    if (end < 0) "/"
    else abspath.substring(0, end + 1)
  }

  /**
   * Returns the name of top-level file or directory of the absolute string path
   *
   * Examples:
   *
   * {{{
   *   basename("/a/b/") == "b"
   *   basename("/a/b") == "b"
   *   basename("/") == ""
   * }}}
   *
   * @param abspath
   *   a string path that matches the syntax of ZipFile entries.
   */
  def basename(abspath: String): String = {
    val end = abspath.lastIndexOf('/')
    val isDir = end == abspath.length - 1
    if (end < 0) abspath
    else if (!isDir) abspath.substring(end + 1)
    else {
      val start = abspath.lastIndexOf('/', end - 1)
      if (start < 0) abspath.substring(0, end)
      else abspath.substring(start + 1, end)
    }
  }

  private def visitJarEntry(
      jarpath: Path,
      isMod: Boolean = false,
      isExcludedPackage: String => Boolean
  ): Unit = {
    val requiredPrefix = if (isMod) "classes/" else ""
    val file = jarpath.toFile
    val jar = new JarFile(file)
    try {
      val entries = jar.entries()
      while (entries.hasMoreElements) {
        val element = entries.nextElement()
        if (
          !element.isDirectory &&
          element.getName.startsWith(requiredPrefix) &&
          !element.getName.startsWith(s"${requiredPrefix}META-INF") &&
          element.getName.endsWith(".class")
        ) {
          val name = element.getName.stripPrefix(requiredPrefix)
          val pkg = dirname(name)
          val member = basename(name)
          addMember(pkg, member)
        }
      }
      if (!isMod) {
        val manifest = jar.getManifest
        if (manifest != null) {
          val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
          if (classpathAttr != null)
            for (relpath <- classpathAttr.split(" "))
              Option(jarpath.getParent)
                .map(_.resolve(relpath))
                .find(abspath =>
                  Files.isRegularFile(abspath) || Files.isDirectory(abspath)
                )
                .foreach(visit(_, isExcludedPackage))
        }
      }
    } finally {
      jar.close()
    }
  }

  def visitBootClasspath(
      javaHome: Path,
      isExcludedPackage: String => Boolean
  ): Unit = {
    val javaVer = JdkVersion0.fromJavaHome(javaHome).getOrElse {
      sys.error(s"Cannot get Java version from $javaHome")
    }
    if (javaVer.major >= 9)
      expandJrtClasspath(javaHome, isExcludedPackage)
    else
      PackageIndex.bootClasspath.foreach(visit(_, isExcludedPackage))
  }

  private def expandJrtClasspath(
      javaHome: Path,
      isExcludedPackage: String => Boolean
  ): Unit = {

    val modsDir = javaHome.resolve("jmods")
    val modFiles = Files
      .list(modsDir)
      .iterator()
      .asScala
      // case-insensitive comparison?
      .filter(_.getFileName.toString.endsWith(".jmod"))
      .filter(Files.isRegularFile(_))
      .toVector

    for (modFile <- modFiles)
      visit(modFile, isExcludedPackage)
  }

}

object PackageIndex {
  def fromClasspath(
      classpath: collection.Seq[Path],
      isExcludedPackage: String => Boolean,
      javaHome: Path
  ): PackageIndex = {
    val packages = new PackageIndex()
    packages.visitBootClasspath(javaHome, isExcludedPackage)
    classpath.foreach { path => packages.visit(path, isExcludedPackage) }
    packages
  }
  def bootClasspath: List[Path] =
    for {
      entries <- sys.props.collectFirst {
        case (k, v) if k.endsWith(".boot.class.path") =>
          v.split(java.io.File.pathSeparator).map(Paths.get(_)).toList
      }.toList
      entry <- entries
      if Files.isRegularFile(entry)
    } yield entry

  private def findJar(name: String) = {
    System
      .getProperty("java.class.path")
      .split(java.io.File.pathSeparator)
      .iterator
      .map(Paths.get(_))
      .filter(path =>
        Files.exists(path) && path.getFileName.toString.contains(name)
      )
      .toSeq
  }

  def scalaLibrary: Seq[Path] = findJar("scala-library")

  def scala3Library: Seq[Path] = findJar("scala3-library")

  def dottyLibrary: Seq[Path] = findJar("dotty-library")

}
