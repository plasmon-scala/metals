package scala.meta.internal.mtags

import scala.meta.Dialect
import scala.meta.dialects
import scala.meta.inputs.Input
import scala.meta.internal.mtags.MtagsEnrichments._
import scala.meta.internal.semanticdb.Language
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.TextDocument
import scala.meta.io.AbsolutePath
import scala.meta.pc.reports.EmptyReportContext
import scala.meta.pc.reports.ReportContext
import java.util.function.Consumer

final class Mtags(implicit rc: ReportContext) {
  def totalLinesOfCode: Long = javaLines + scalaLines
  def totalLinesOfScala: Long = scalaLines
  def totalLinesOfJava: Long = javaLines

  def allSymbols(
      input: Input.VirtualFile,
      dialectOpt: Option[Dialect]
  ): TextDocument =
    index(input.toLanguage, input, dialectOpt)

  def toplevels(
      path: AbsolutePath,
      dialect: Dialect = dialects.Scala213,
      logger: Consumer[String] = null
  ): TextDocument =
    toplevels(path.toInput, dialect, logger = logger)

  def toplevels(
      input: Input.VirtualFile,
      dialect: Dialect,
      logger: Consumer[String]
  ): TextDocument = {
    val language = input.toLanguage

    if (language.isJava || language.isScala) {
      val mtags =
        if (language.isJava)
          new JavaToplevelMtags(input, includeInnerClasses = false)
        else
          new ScalaToplevelMtags(
            input,
            includeInnerClasses = false,
            includeMembers = false,
            dialect,
            logger = logger
          )
      addLines(language, input.text)
      Mtags.stdLibPatches.patchDocument(
        input.path,
        mtags.index()
      )
    } else {
      TextDocument()
    }
  }

  def indexWithOverrides(
      input: Input.VirtualFile,
      dialectOpt: Option[Dialect] = None,
      includeMembers: Boolean = false,
      logger: Consumer[String] = null
  ): (TextDocument, MtagsIndexer.AllOverrides) = {
    val language = input.toLanguage
    if (language.isJava) {
      val mtags = new JavaToplevelMtags(input, includeInnerClasses = true)
      addLines(language, input.text)
      val doc = Mtags.stdLibPatches.patchDocument(
        input.path,
        mtags.index()
      )
      (doc, mtags.overrides())
    } else if (language.isScala)
      dialectOpt match {
        case Some(dialect) =>
          val mtags = new ScalaToplevelMtags(
            input,
            includeInnerClasses = true,
            includeMembers,
            dialect,
            logger = logger
          )
          addLines(language, input.text)
          val doc =
            Mtags.stdLibPatches.patchDocument(
              input.path,
              mtags.index()
            )
          val overrides = mtags.overrides()
          (doc, overrides)
        case None =>
          (TextDocument(), Nil)
      }
    else
      (TextDocument(), Nil)
  }

  def topLevelSymbols(
      input: Input.VirtualFile
  ): List[String] = {
    topLevelSymbols(input, dialects.Scala213)
  }

  def topLevelSymbols(
      input: Input.VirtualFile,
      dialect: Dialect
  ): List[String] =
    topLevelSymbols(input, dialect, logger = null)

  def topLevelSymbols(
      input: Input.VirtualFile,
      dialect: Dialect,
      logger: Consumer[String]
  ): List[String] = {
    toplevels(input, dialect, logger).occurrences.iterator
      .filterNot(_.symbol.isPackage)
      .map(_.symbol)
      .toList
  }

  def topLevelSymbols(
      path: AbsolutePath,
      dialect: Dialect = dialects.Scala213
  ): List[String] =
    topLevelSymbols(path.toInput, dialect)

  def index(
      language: Language,
      input: Input.VirtualFile,
      dialectOpt: Option[Dialect]
  ): TextDocument = {
    addLines(language, input.text)
    val result =
      if (language.isJava)
        JavaMtags
          .index(input, includeMembers = true)
          .index()
      else if (language.isScala)
        dialectOpt match {
          case Some(dialect) =>
            ScalaMtags.index(input, dialect).index()
          case None =>
            TextDocument()
        }
      else
        TextDocument()
    Mtags.stdLibPatches
      .patchDocument(
        input.path,
        result
      )
      .withUri(input.path)
      .withText(input.text)
  }
  private var javaLines: Long = 0L
  private var scalaLines: Long = 0L
  private def addLines(language: Language, text: String): Unit = {
    if (language.isJava) {
      javaLines += text.linesIterator.length
    } else if (language.isScala) {
      scalaLines += text.linesIterator.length
    }
  }
}
object Mtags {
  def index(path: AbsolutePath, dialectOpt: Option[Dialect])(implicit
      rc: ReportContext = new EmptyReportContext()
  ): TextDocument = {
    new Mtags().index(path.toLanguage, path.toInput, dialectOpt)
  }

  def index(path: SourcePath, dialectOpt: Option[Dialect])(implicit
      rc: ReportContext,
      ctx: SourcePath.Context
  ): TextDocument = {
    new Mtags().index(path.toLanguage, path.toInput, dialectOpt)
  }

  def toplevels(document: TextDocument): List[String] = {
    document.occurrences.iterator
      .filter { occ =>
        occ.role.isDefinition &&
        Symbol(occ.symbol).isToplevel
      }
      .map(_.symbol)
      .toList
  }

  def allToplevels(
      input: Input.VirtualFile,
      dialect: Dialect,
      includeMembers: Boolean = true,
      logger: Consumer[String] = null
  )(implicit rc: ReportContext = new EmptyReportContext()): TextDocument =
    input.toLanguage match {
      case Language.JAVA =>
        new JavaMtags(input, includeMembers = true).index()
      case Language.SCALA =>
        val mtags =
          new ScalaToplevelMtags(
            input,
            true,
            includeMembers,
            dialect,
            logger = logger
          )
        mtags.index()
      case _ =>
        TextDocument()
    }

  def toplevels(
      path: AbsolutePath,
      dialect: Dialect = dialects.Scala213
  )(implicit rc: ReportContext = new EmptyReportContext()): TextDocument = {
    new Mtags().toplevels(path, dialect)
  }

  def indexWithOverrides(
      path: Input.VirtualFile,
      dialectOpt: Option[Dialect] = None,
      includeMembers: Boolean = false
  )(implicit
      rc: ReportContext = new EmptyReportContext()
  ): (TextDocument, MtagsIndexer.AllOverrides) = {
    new Mtags().indexWithOverrides(path, dialectOpt, includeMembers)
  }

  def topLevelSymbols(
      path: AbsolutePath,
      dialect: Dialect = dialects.Scala213
  )(implicit rc: ReportContext = new EmptyReportContext()): List[String] = {
    new Mtags().topLevelSymbols(path, dialect)
  }

  /**
   * Scala 3 has a specific package that adds / replaces some symbols in scala.Predef + scala.language
   * https://github.com/lampepfl/dotty/blob/main/library/src/scala/runtime/stdLibPatches/
   * We need to do the same to correctly provide location for symbols obtained from semanticdb.
   */
  private object stdLibPatches {
    val packageName = "scala/runtime/stdLibPatches"

    private def isScala3Library(jar: AbsolutePath): Boolean =
      jar.filename.startsWith("scala3-library_3")

    private def isScala3LibraryPatchSource(path: String): Boolean =
      SourcePath(path) match {
        case z: SourcePath.ZipEntry =>
          isScala3Library(AbsolutePath(z.zipPath)) && {
            val segments = z.pathInZip.split("/")
            segments.length >= 2 &&
            segments(segments.length - 2) == "stdLibPatches"
          }
        case _: SourcePath.Standard =>
          false
      }

    private def patchSymbol(sym: String): String =
      sym.replace(packageName, "scala")

    def patchDocument(
        path: String,
        doc: TextDocument
    ): TextDocument = {
      if (isScala3LibraryPatchSource(path)) {
        val occs =
          doc.occurrences.map(occ => occ.copy(symbol = patchSymbol(occ.symbol)))

        doc.copy(occurrences = occs)
      } else doc
    }

  }

}
