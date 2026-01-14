package scala.meta.internal.mtags

import java.nio.file.Path

import scala.collection.mutable

final class OpenClassLoader {

  private val isAdded = mutable.Set.empty[Path]
  private val list0 = mutable.ListBuffer.empty[Path]

  override def toString: String =
    list0.iterator.mkString("OpenClassLoader(", ", ", ")")

  def addClasspath(classpath: List[Path]): Boolean =
    classpath.forall(addEntry)

  def addEntry(entry: Path): Boolean = {
    val isAdded0 = isAdded(entry)
    if (!isAdded0) {
      list0 += entry
      isAdded += entry
    }
    !isAdded0
  }

  def hasEntry(entry: Path): Boolean =
    isAdded(entry)

  def loadClassSafe(symbol: String): Option[Class[_]] =
    None

  def list(): Seq[Path] =
    list0.toList

  def duplicate(): OpenClassLoader = {
    val copy = new OpenClassLoader
    copy.isAdded ++= isAdded
    copy.list0 ++= list0
    copy
  }

}
