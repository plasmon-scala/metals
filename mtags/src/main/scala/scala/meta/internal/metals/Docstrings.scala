package scala.meta.internal.metals

import java.util.Optional
import java.util.logging.Level
import java.util.logging.Logger

import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal

import scala.meta.Dialect
import scala.meta.inputs.Input
import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.mtags.OnDemandSymbolIndex
import scala.meta.internal.mtags.ScalaMtags
import scala.meta.internal.mtags.ScalametaCommonEnrichments._
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.mtags.SymbolDefinition
import scala.meta.internal.semanticdb.Language
import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.internal.semanticdb.SymbolOccurrence
import scala.meta.io.AbsolutePath
import scala.meta.pc.ContentType
import scala.meta.pc.ContentType.MARKDOWN
import scala.meta.pc.ContentType.PLAINTEXT
import scala.meta.pc.ParentSymbols
import scala.meta.pc.SymbolDocumentation
import scala.meta.pc.reports.ReportContext
import java.nio.file.Path
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

/**
 * Implementation of the `documentation(symbol: String): Option[SymbolDocumentation]` method in `SymbolSearch`.
 *
 * Handles both javadoc and scaladoc.
 */
class Docstrings(index: GlobalSymbolIndex)(implicit rc: ReportContext) {
  val cache = new TrieMap[Content, SymbolDocumentation]()
  private val logger0 = Logger.getLogger(classOf[Docstrings].getName)

  def documentation(
      symbol: String,
      parents: ParentSymbols,
      contentType: ContentType,
      logger: java.util.function.Consumer[String]
  ): Optional[SymbolDocumentation] = {
    val result = getFromCacheWithProxy(symbol, contentType) match {
      case Some(value) =>
        if (value == EmptySymbolDocumentation) None
        else Some(value)
      case None =>
        indexSymbol(symbol, contentType, logger)
        val result = getFromCacheWithProxy(symbol, contentType)
        if (result.isEmpty)
          cache(Content.from(symbol, contentType)) = EmptySymbolDocumentation
        result
    }
    /* Fall back to parent javadocs/scaladocs if nothing is specified for the current symbol
     * This way we also cache the result in order not to calculate parents again.
     */
    val resultWithParentDocs = result match {
      case Some(value: MetalsSymbolDocumentation)
          if value.docstring.isEmpty() =>
        Some(parentDocumentation(symbol, value, parents, contentType, logger))
      case None =>
        Some(
          parentDocumentation(
            symbol,
            MetalsSymbolDocumentation.empty(symbol),
            parents,
            contentType,
            logger
          )
        )
      case _ => result
    }
    Optional.ofNullable(resultWithParentDocs.orNull)
  }

  def parentDocumentation(
      symbol: String,
      docs: MetalsSymbolDocumentation,
      parents: ParentSymbols,
      contentType: ContentType,
      logger: java.util.function.Consumer[String]
  ): SymbolDocumentation = {
    parents
      .parents()
      .asScala
      .flatMap { s =>
        getFromCacheWithProxy(s, contentType).orElse {
          indexSymbol(s, contentType, logger)
          getFromCacheWithProxy(s, contentType)
        }
      }
      .find(_.docstring().nonEmpty)
      .fold {
        docs
      } { withDocs =>
        val updated = docs.copy(docstring = withDocs.docstring())
        cache(Content.from(symbol, contentType)) = updated
        updated
      }
  }

  private def getFromCacheWithProxy(
      symbol: String,
      contentType: ContentType
  ): Option[SymbolDocumentation] = {
    cache.get(Content.from(symbol, contentType)) match {
      case Some(ProxySymbolDocumentation(alternativeSymbol)) =>
        cache.get(Content.from(alternativeSymbol, contentType))
      case res => res
    }
  }

  /**
   * Expire all symbols showed in the given scala source file.
   *
   * Note that what this method does is only expiring the cache, and
   * it doesn't update the cache in honor of the memory footprint.
   * Otherwise, if we update the cache for symbols every time we save a file,
   * metals will cache all symbols in files we've saved, and it consumes a considerable amount of memory.
   *
   * @param path the absolute path for the source file to update.
   */
  def expireSymbolDefinition(path: AbsolutePath, dialect: Dialect): Unit = {
    path.toLanguage match {
      case Language.SCALA =>
        new Deindexer(path.toInput, dialect).indexRoot()
      case _ =>
    }
  }

  private def cacheSymbol(
      doc: SymbolDocumentation,
      contentType: ContentType
  ): Unit = {
    cache(Content.from(doc.symbol(), contentType)) = doc
  }

  private def indexSymbol(
      symbol: String,
      contentType: ContentType,
      logger: java.util.function.Consumer[String]
  ): Unit = {
    index.definition(Symbol(symbol)) match {
      case Some(defn) =>
        try {
          if (logger != null)
            logger.accept(s"Indexing javadoc / scaladoc of $symbol")
          indexSymbolDefinition(defn, contentType)
          maybeCacheAlternative(defn, contentType)
          if (logger != null) logger.accept("Done indexing javadoc / scaladoc")
        } catch {
          case NonFatal(e) =>
            logger0.log(Level.SEVERE, defn.path.toURI.toString, e)
            if (logger != null) {
              logger.accept(
                s"Error while indexing javadoc / scaladoc of $symbol"
              )
              val baos = new ByteArrayOutputStream
              e.printStackTrace(
                new java.io.PrintStream(baos, true, StandardCharsets.UTF_8)
              )
              logger.accept(
                new String(baos.toByteArray, StandardCharsets.UTF_8)
              )
            }
        }
      case None =>
    }
  }

  private def maybeCacheAlternative(
      defn: SymbolDefinition,
      contentType: ContentType
  ) = {
    val defSymbol = defn.definitionSymbol.value
    val querySymbol = defn.querySymbol.value
    lazy val queryContent = Content.from(querySymbol, contentType)

    if (
      defSymbol != querySymbol && cache.get(queryContent).forall {
        case EmptySymbolDocumentation | _: ProxySymbolDocumentation => true
        case _ => false
      }
    ) cache.put(queryContent, new ProxySymbolDocumentation(defSymbol))
  }

  private def indexSymbolDefinition(
      defn: SymbolDefinition,
      contentType: ContentType
  ): Unit = {
    defn.path.toLanguage match {
      case Language.JAVA =>
        JavadocIndexer
          .foreach(defn.path.toInput, contentType)(cacheSymbol(_, contentType))
      case Language.SCALA =>
        ScaladocIndexer
          .foreach(defn.path.toInput, defn.dialect, contentType)(
            cacheSymbol(_, contentType)
          )
      case _ =>
    }
  }

  private class Deindexer(
      input: Input.VirtualFile,
      dialect: Dialect
  ) extends ScalaMtags(input, dialect) {
    override def visitOccurrence(
        occ: SymbolOccurrence,
        sinfo: SymbolInformation,
        owner: String
    ): Unit = {
      for {
        contentType <- ContentType.values()
      } cache.remove(Content.from(sinfo.symbol, contentType))
    }
  }

}

object Docstrings {
  def empty(javaHome: Path)(implicit rc: ReportContext): Docstrings =
    new Docstrings(
      OnDemandSymbolIndex.empty(javaHome)
    )
}

sealed trait Content extends Any
class Markdown(val text: String) extends AnyVal with Content
class Plain(val text: String) extends AnyVal with Content

object Content {
  def from(text: String, contentType: ContentType): Content =
    contentType match {
      case MARKDOWN => new Markdown(text)
      case PLAINTEXT => new Plain(text)
    }
}
