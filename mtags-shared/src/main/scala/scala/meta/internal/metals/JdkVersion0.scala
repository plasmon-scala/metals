package scala.meta.internal.metals

import java.nio.file.{Files, Path}
import java.{util => ju}

import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.Try

case class JdkVersion0(
    major: Int,
    full: String
)

object JdkVersion0 {

  def fromJavaHome(
      javaHome: Path
  ): Option[JdkVersion0] =
    fromReleaseFile(javaHome)

  def fromReleaseFile(javaHome: Path): Option[JdkVersion0] =
    Iterator(javaHome.resolve("release"), javaHome.getParent.resolve("release"))
      .filter(Files.exists(_))
      .flatMap { releaseFile =>
        val props = new ju.Properties
        props.load(Source.fromFile(releaseFile.toFile).bufferedReader())
        props.asScala
          .get("JAVA_VERSION")
          .iterator
          .map(_.stripPrefix("\"").stripSuffix("\""))
          .flatMap(JdkVersion0.parse)
      }
      .take(1)
      .toList
      .headOption

  def parse(v: String): Option[JdkVersion0] = {
    val numbers = Try {
      v
        .split('-')
        .head
        .split('.')
        .toList
        .take(2)
        .flatMap(s => Try(s.toInt).toOption)
    }.toOption

    numbers match {
      case Some(1 :: minor :: _) =>
        Some(JdkVersion0(minor, v))
      case Some(single :: _) =>
        Some(JdkVersion0(single, v))
      case _ => None
    }
  }
}
