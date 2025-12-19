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

final class OpenClassLoader extends AutoCloseable {

  // from https://github.com/scalameta/scalameta/blob/491b8b28b6e7c3a75994bba4caf5c4b4fee526a2/scalameta/io/jvm/src/main/scala/scala/meta/internal/io/PlatformFileIO.scala#L103-L105
  private def newFileSystem(path: Path): FileSystem = {
    val uri = URI.create("jar:" + path.toUri.toString)
    try FileSystems.newFileSystem(uri, new util.HashMap[String, Object])
    catch {
      case _: FileSystemAlreadyExistsException => FileSystems.getFileSystem(uri)
    }
  }

  override def close(): Unit =
    for ((_, path) <- map)
      path.getFileSystem.close()

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
      .flatMap { case (_, root) =>
        val f = root.resolve(uri)
        val exists = Files.exists(f)
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
