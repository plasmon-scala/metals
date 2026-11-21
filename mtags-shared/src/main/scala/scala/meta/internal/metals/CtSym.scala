package scala.meta.internal.metals

import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.ZipFile

import scala.collection.mutable

/**
 * The class signatures of `lib/ct.sym`, the file javac reads for `--release`.
 *
 * Every JDK 9+ that has javac ships it, unlike `jmods/`, which JDKs built
 * without JMODs (JEP 493, like Temurin 24 and later) don't have. It's a plain
 * ZIP file, with one `.sig` file (a class file without code) per class, under
 * `<releases>/<module>/<package path>/`, where `<releases>` lists the releases
 * the signature is valid for, one character each (`8`, `9`, then `A` for 10,
 * `B` for 11, …, `O` for 24).
 *
 * [[CtSym.layout]] picks the highest release that has class signatures. That's
 * the JDK's own release since JDK 22. Up to JDK 21, ct.sym only has the
 * signatures of the previous releases (the directory of the JDK's own release
 * just has a `system-modules` file, javac reads the JDK's own classes for it).
 */
object CtSym {

  def isCtSym(path: Path): Boolean = {
    val name = path.getFileName
    name != null && name.toString == "ct.sym"
  }

  val classSuffix = ".sig"

  /** The character of a release in ct.sym: `8`, `9`, then `A` for 10, … */
  def releaseChar(major: Int): Char =
    if (major < 10) ('0' + major).toChar
    else ('A' + (major - 10)).toChar

  /**
   * @param release
   *   the release character the signatures are those of
   * @param modules
   *   for each module, the entry prefixes (like `"MNO/java.base/"`) its
   *   signatures for `release` are under
   */
  final case class Layout(release: Char, modules: Map[String, Seq[String]]) {
    lazy val prefixes: Seq[String] = modules.valuesIterator.flatten.toVector

    /** `"java/lang/String.sig"` for `"MNO/java.base/java/lang/String.sig"` */
    def relativePath(entryName: String): Option[String] =
      prefixes.collectFirst {
        case prefix if entryName.startsWith(prefix) =>
          entryName.substring(prefix.length)
      }
  }

  private val layoutCache = new ConcurrentHashMap[Path, Layout]

  /** The layout of the signatures of the JDK the ct.sym file comes from */
  def layout(ctSym: Path): Layout = {
    val valueOrNull = layoutCache.get(ctSym)
    if (valueOrNull == null) {
      val layout0 = computeLayout(ctSym)
      val previousOrNull = layoutCache.putIfAbsent(ctSym, layout0)
      if (previousOrNull == null) layout0
      else previousOrNull
    } else valueOrNull
  }

  private def isReleaseChar(c: Char): Boolean =
    (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z')

  // Directories like "9-modules" aren't class signatures
  private def isReleasesDir(name: String): Boolean =
    name.nonEmpty && name.forall(isReleaseChar)

  private def computeLayout(ctSym: Path): Layout = {
    val zf = new ZipFile(ctSym.toFile)
    // releases dir -> modules in it
    val dirs = new mutable.HashMap[String, mutable.Set[String]]
    try {
      val entries = zf.entries()
      while (entries.hasMoreElements) {
        val name = entries.nextElement().getName
        val firstSlash = name.indexOf('/')
        if (firstSlash > 0) {
          val secondSlash = name.indexOf('/', firstSlash + 1)
          val releases = name.substring(0, firstSlash)
          if (secondSlash > firstSlash + 1 && isReleasesDir(releases))
            dirs
              .getOrElseUpdate(releases, new mutable.HashSet[String])
              .add(name.substring(firstSlash + 1, secondSlash))
        }
      }
    } finally zf.close()
    if (dirs.isEmpty)
      Layout('0', Map.empty)
    else {
      val release = dirs.keysIterator.flatMap(_.iterator).max
      val prefixes = for {
        (releases, modules) <- dirs.toVector
        if releases.indexOf(release) >= 0
        module <- modules.toVector
      } yield (module, s"$releases/$module/")
      Layout(
        release,
        prefixes
          .groupBy(_._1)
          .map { case (module, values) => (module, values.map(_._2).sorted) }
      )
    }
  }
}
