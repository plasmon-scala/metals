package scala.meta.internal.mtags

import java.io.FileNotFoundException
import java.net.URI
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters._
import scala.meta.internal.mtags.CommonMtagsEnrichments._

sealed abstract class SourcePath extends Product with Serializable {
  def uri: String
  def filePath: Option[Path]
  def content(charSet: Charset = StandardCharsets.UTF_8)(implicit
      context: SourcePath.Context
  ): String

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
      Files.readString(path, charSet)
    def exists()(implicit context: SourcePath.Context): Boolean =
      Files.exists(path)
    override def isScalaScript: Boolean =
      path.toString.isScalaScript
    override def isMill: Boolean =
      path.toString.isMill
    def extension: String = {
      val fileName = path.getFileName.toString
      val idx = fileName.lastIndexOf('.')
      if (idx == -1) ""
      else fileName.substring(idx + 1)
    }
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
