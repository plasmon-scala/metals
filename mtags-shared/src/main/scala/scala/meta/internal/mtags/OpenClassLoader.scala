package scala.meta.internal.mtags

import java.nio.file.Path

import scala.collection.mutable

import scala.meta.internal.mtags.CommonMtagsEnrichments.XtensionNIOPath
import java.net.URI
import java.util
import java.nio.file.FileSystemAlreadyExistsException
import java.nio.file.FileSystem
import java.nio.file.FileSystems
import java.nio.file.Files

final class OpenClassLoader {

  // from https://github.com/scalameta/scalameta/blob/491b8b28b6e7c3a75994bba4caf5c4b4fee526a2/scalameta/io/jvm/src/main/scala/scala/meta/internal/io/PlatformFileIO.scala#L103-L105
  private def newFileSystem(path: Path): FileSystem = {
    val uri = URI.create("jar:" + path.toUri.toString)
    try FileSystems.newFileSystem(uri, new util.HashMap[String, Object])
    catch {
      case _: FileSystemAlreadyExistsException => FileSystems.getFileSystem(uri)
    }
  }

  private val isAdded = mutable.Set.empty[Path]
  private val map = mutable.ListMap.empty[Path, Path]

  override def toString: String =
    map.iterator.map(_._1).mkString("OpenClassLoader(", ", ", ")")

  def addClasspath(classpath: List[Path]): Boolean =
    classpath.forall(addEntry)

  def addEntry(entry: Path): Boolean = {
    val isAdded0 = isAdded(entry)
    if (!isAdded0) {
      val fs = newFileSystem(entry)
      map += entry -> fs.getPath("/")
      isAdded += entry
    }
    !isAdded0
  }

  private def resolve0(uri: String): Iterator[Path] =
    map.iterator
      .flatMap { case (entry, root) =>
        val (root0, f, exists) =
          try {
            val f0 = root.resolve(uri)
            val exists0 = Files.exists(f0)
            (root, f0, exists0)
          } catch {
            case _: java.nio.file.ClosedFileSystemException =>
              // throw new Exception(s"Error accessing ${f.getFileSystem}", ex)
              // Sometimes happens for the jline JAR, not sure why…
              val fs = newFileSystem(entry)
              val root0 = fs.getPath("/")
              map += entry -> root0
              val f0 = root0.resolve(uri)
              val exists0 = Files.exists(f0)
              (root0, f0, exists0)
          }
        if (exists) Iterator(f)
        else Iterator.empty
      }
  private def resolve(uri: String): Option[Path] =
    resolve0(uri).take(1).toList.headOption
  def resolveAll(uri: String): List[Path] =
    resolve0(uri).toList

  def resolve(relpath: Path): Option[Path] = {
    val uri = relpath.toURI(isDirectory = false).toString
    resolve(uri)
  }

  def loadClassSafe(symbol: String): Option[Class[_]] =
    None

}
