package scala.meta.internal.mtags

import java.util.concurrent.ConcurrentHashMap

import scala.meta.Dialect
import scala.meta.dialects
import scala.meta.inputs.Input
import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.jsemanticdb.Semanticdb
import scala.meta.internal.metals.EmptyReportContext
import scala.meta.internal.metals.ReportContext
import scala.meta.internal.mtags.ScalametaCommonEnrichments._
import scala.meta.internal.semanticdb.Language
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Schema
import scala.meta.internal.semanticdb.TextDocument
import scala.meta.io.AbsolutePath

final class Mtags(val config: MtagsConfig = MtagsConfig.default)(implicit
    val rc: ReportContext
) {
  def totalLinesOfCode: Long = lineCounts.values.asScala.sum
  def totalSymbols: Long = symbolsCount
  def totalLinesOfScala: Long =
    lineCounts.getOrDefault(Semanticdb.Language.SCALA, 0L)
  def totalLinesOfProtobuf: Long =
    lineCounts.getOrDefault(Semanticdb.Language.PROTOBUF, 0L)
  def totalLinesOfJava: Long =
    lineCounts.getOrDefault(Semanticdb.Language.JAVA, 0L)

  def allSymbols(path: AbsolutePath, dialect: Dialect): TextDocument = {
    index(path, dialect)
  }

  def allSymbols(input: Input.VirtualFile, dialect: Dialect): TextDocument =
    index(input.toJLanguage, input, dialect)

  def toplevels(
      path: AbsolutePath,
      dialect: Dialect = dialects.Scala213
  ): TextDocument =
    toplevels(path.toInput, dialect)

  def toplevels(
      input: Input.VirtualFile,
      dialect: Dialect
  ): TextDocument = {
    val language = input.toJLanguage

    if (language.isJava || language.isScala) {
      val indexer =
        if (language.isJava)
          new JavaToplevelMtags(input, includeInnerClasses = false)
        else
          new ScalaToplevelMtags(
            input,
            includeInnerClasses = false,
            includeMembers = false,
            dialect
          )
      addLines(language, input.text)
      Mtags.stdLibPatches.patchDocument(
        input.path,
        indexer.index()
      )
    } else if (language.isProtobuf) {
      config.protoInstance(input, includeGeneratedSymbols = true).index()
    } else {
      TextDocument()
    }
  }

  def extendedIndexing(
      input: Input.VirtualFile,
      dialect: Dialect = dialects.Scala213,
      includeMembers: Boolean = false
  ): (
      TextDocument,
      MtagsIndexer.AllOverrides,
      MtagsIndexer.AllToplevelMembers
  ) = {
    val language = input.toJLanguage
    if (language.isJava || language.isScala) {
      val indexer =
        if (language.isJava)
          if (includeMembers)
            config.javaInstance(input, includeMembers = true)
          else
            new JavaToplevelMtags(input, includeInnerClasses = true)
        else if (language.isScala)
          new ScalaToplevelMtags(
            input,
            includeInnerClasses = true,
            includeMembers,
            dialect
          )
        else
          new EmptyToplevelMtags(input)

      addLines(language, input.text)
      val doc =
        Mtags.stdLibPatches.patchDocument(
          input.path,
          indexer.index()
        )
      val overrides = indexer.overrides()
      val toplevelMembers = indexer.toplevelMembers()
      (doc, overrides, toplevelMembers)
    } else if (language.isProtobuf) {
      config.protoInstance(input, includeGeneratedSymbols = true).index()
      (TextDocument(), Nil, Nil)
    } else {
      (TextDocument(), Nil, Nil)
    }
  }

  def topLevelSymbols(
      input: Input.VirtualFile
  ): List[String] = {
    topLevelSymbols(input, dialects.Scala213)
  }

  def topLevelSymbols(
      input: Input.VirtualFile,
      dialect: Dialect
  ): List[String] = {
    toplevels(input, dialect).occurrences.iterator
      .filterNot(_.symbol.isPackage)
      .map(_.symbol)
      .toList
  }

  def topLevelSymbols(
      path: AbsolutePath,
      dialect: Dialect = dialects.Scala213
  ): List[String] =
    topLevelSymbols(path.toInput, dialect)

  def allToplevels(
      input: Input.VirtualFile,
      dialect: Dialect,
      includeMembers: Boolean = true
  )(implicit rc: ReportContext = EmptyReportContext): TextDocument =
    input.toLanguage match {
      case Language.JAVA =>
        config.javaInstance(input, includeMembers = true).index()
      case Language.SCALA =>
        val mtags =
          new ScalaToplevelMtags(input, true, includeMembers, dialect)
        mtags.index()
      case _ =>
        TextDocument()
    }

  def index(
      path: AbsolutePath,
      dialect: Dialect
  ): TextDocument =
    index(path.toJLanguage, path.toInput, dialect)

  def index(
      language: Semanticdb.Language,
      input: Input.VirtualFile,
      dialect: Dialect
  ): TextDocument = {
    addLines(language, input.text)
    val result = index(input, dialect)
    Mtags.stdLibPatches
      .patchDocument(
        input.path,
        result
      )
      .withUri(input.path)
      .withText(input.text)
  }

  /**
   * Index a text document on the fly, typically on decompiled code
   */
  def index(
      input: Input.VirtualFile,
      dialect: Dialect
  ): TextDocument = {
    val language = input.toJLanguage
    val result =
      if (language == Semanticdb.Language.JAVA) {
        config.javaInstance(input, includeMembers = true).index()
      } else if (language == Semanticdb.Language.SCALA) {
        ScalaMtags.index(input, dialect).index()
      } else if (language == Semanticdb.Language.PROTOBUF) {
        config.protoInstance(input, includeGeneratedSymbols = true).index()
      } else {
        TextDocument()
      }
    result
  }

  def indexMBT(
      language: Semanticdb.Language,
      input: Input.VirtualFile,
      dialect: Dialect,
      includeReferences: Boolean = false
  ): TextDocument =
    indexMBTWithOverrides(language, input, dialect, includeReferences)._1

  def indexMBTWithOverrides(
      language: Semanticdb.Language,
      input: Input.VirtualFile,
      dialect: Dialect,
      includeReferences: Boolean = false
  ): (TextDocument, MtagsIndexer.AllOverrides) = {
    addLines(language, input.text)
    val indexer: Option[MtagsIndexer] =
      if (language == Semanticdb.Language.JAVA) {
        Some(
          config.javaInstance(
            input,
            includeMembers = true,
            includeReferences = includeReferences
          )
        )
      } else if (language == Semanticdb.Language.SCALA) {
        Some(
          new ScalaToplevelMtags(
            input,
            includeInnerClasses = true,
            includeMembers = true,
            dialect,
            collectIdentifiers = includeReferences
          )
        )
      } else if (language == Semanticdb.Language.PROTOBUF) {
        Some(
          config.protoInstance(
            input,
            includeGeneratedSymbols = true,
            includeFuzzyReferences = includeReferences
          )
        )
      } else {
        None
      }
    val result = indexer.fold(TextDocument())(_.index())
    symbolsCount += result.occurrences.length
    val doc = result
      .withUri(input.path)
      .withText("")
      .withSchema(Schema.SEMANTICDB4)
      // NOTE: we loose the protobuf language here, can only recover it from the URI.
      .withLanguage(language.toLanguage)
    (doc, indexer.fold(Nil: MtagsIndexer.AllOverrides)(_.overrides()))
  }
  private var symbolsCount: Long = 0L
  private val lineCounts = new ConcurrentHashMap[Semanticdb.Language, Long]

  private def addLines(language: Semanticdb.Language, text: String): Unit = {
    val linesCount = text.linesIterator.length
    lineCounts.compute(
      language,
      (_, existingLinesCount) =>
        linesCount + Option(existingLinesCount).getOrElse(0L)
    )
  }
}
object Mtags {
  lazy val testingSingleton: Mtags = new Mtags()(EmptyReportContext)

  def index(path: SourcePath, dialect: Dialect)(implicit
      rc: ReportContext,
      ctx: SourcePath.Context
  ): TextDocument = {
    val input = path.toInput
    new Mtags().index(input.toJLanguage, input, dialect)
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
      path.startsWith("jar:") && (SourcePath(path) match {
        case z: SourcePath.ZipEntry =>
          isScala3Library(AbsolutePath(z.zipPath)) && {
            val segments = z.pathInZip.split("/")
            segments.length >= 2 &&
            segments(segments.length - 2) == "stdLibPatches"
          }
        case _: SourcePath.Standard =>
          false
      })

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
