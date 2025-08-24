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
import scala.meta.internal.mtags.Mtags
import scala.meta.internal.mtags.OnDemandSymbolIndex
import scala.meta.internal.mtags.ScalaMtags
import scala.meta.internal.mtags.MtagsEnrichments._
import scala.meta.internal.mtags.SourcePath
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
import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

/**
 * Implementation of the `documentation(symbol: String): Option[SymbolDocumentation]` method in `SymbolSearch`.
 *
 * Handles both javadoc and scaladoc.
 */
class Docstrings(
    index: GlobalSymbolIndex,
    indexInBackground: Boolean = false
)(implicit rc: ReportContext) {
  val cache =
    new TrieMap[(Content, GlobalSymbolIndex.Module), SymbolDocumentation]()
  private val logger0 = Logger.getLogger(classOf[Docstrings].getName)

  private lazy val pool = Executors.newSingleThreadExecutor(
    new ThreadFactory {
      val count = new AtomicInteger
      override def newThread(r: Runnable): Thread = {
        val t = new Thread(r, s"docstrings-${count.incrementAndGet()}")
        t.setDaemon(true)
        t
      }
    }
  )

  def reset(): Unit =
    cache.clear()

  def documentation(
      module: GlobalSymbolIndex.Module,
      symbol: String,
      parents: ParentSymbols,
      contentType: ContentType,
      logger: java.util.function.Consumer[String]
  )(implicit ctx: SourcePath.Context): Optional[SymbolDocumentation] = {
    val content = Content.from(symbol, contentType)
    if (!indexInBackground && !cache.contains((content, module)))
      new IndexSymbol(content, symbol, module, contentType, logger).run()

    getFromCacheWithProxy(module, symbol, contentType) match {
      case None =>
        val runnable =
          new IndexSymbol(content, symbol, module, contentType, logger)
        if (indexInBackground) pool.submit(runnable)
        Optional.of(MetalsSymbolDocumentation.ongoing(symbol))
      case Some(result) =>
        /* Fall back to parent javadocs/scaladocs if nothing is specified for the current symbol
         * This way we also cache the result in order not to calculate parents again.
         */
        val resultWithParentDocs = result match {
          case value: MetalsSymbolDocumentation if value.docstring.isEmpty() =>
            parentDocumentation(
              module,
              symbol,
              value,
              parents,
              contentType,
              logger
            )
          case EmptySymbolDocumentation =>
            parentDocumentation(
              module,
              symbol,
              MetalsSymbolDocumentation.empty(symbol),
              parents,
              contentType,
              logger
            )
          case _ => result
        }
        // scribe.info(s"resultWithParentDocs=$resultWithParentDocs")
        Optional.ofNullable(resultWithParentDocs)
    }
  }

  def parentDocumentation(
      module: GlobalSymbolIndex.Module,
      symbol: String,
      docs: MetalsSymbolDocumentation,
      parents: ParentSymbols,
      contentType: ContentType,
      logger: java.util.function.Consumer[String]
  )(implicit ctx: SourcePath.Context): SymbolDocumentation = {
    val parentsDoc = parents
      .parents()
      .asScala
      .map { s =>
        val content = Content.from(s, contentType)
        val onGoing = !cache.contains((content, module)) && {
          val runnable =
            new IndexSymbol(content, s, module, contentType, logger)
          if (indexInBackground) pool.submit(runnable)
          else runnable.run()
          indexInBackground
        }
        (onGoing, getFromCacheWithProxy(module, s, contentType))
      }
    parentsDoc
      .flatMap(_._2)
      .find(_.docstring().nonEmpty)
      .fold {
        if (parentsDoc.exists(_._1))
          docs.copy(docstring = "...")
        else
          docs
      } { withDocs =>
        val updated = docs.copy(docstring = withDocs.docstring())
        cache((Content.from(symbol, contentType), module)) = updated
        updated
      }
  }

  private def getFromCacheWithProxy(
      module: GlobalSymbolIndex.Module,
      symbol: String,
      contentType: ContentType
  ): Option[SymbolDocumentation] = {
    cache.get((Content.from(symbol, contentType), module)) match {
      case Some(ProxySymbolDocumentation(alternativeSymbol)) =>
        cache.get((Content.from(alternativeSymbol, contentType), module))
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
  def expireSymbolDefinition(
      module: GlobalSymbolIndex.Module,
      path: AbsolutePath,
      dialectOpt: Option[Dialect]
  ): Unit = {
    path.toLanguage match {
      case Language.SCALA =>
        for (dialect <- dialectOpt)
          new Deindexer(module, path.toInput, dialect).indexRoot()
      case _ =>
    }
  }

  private def cacheSymbol(
      module: GlobalSymbolIndex.Module,
      doc: SymbolDocumentation,
      contentType: ContentType
  ): Unit = {
    // scribe.info("doc=" + pprint.apply(doc))
    cache((Content.from(doc.symbol(), contentType), module)) = doc
  }

  private def indexSymbol(
      module: GlobalSymbolIndex.Module,
      symbol: String,
      contentType: ContentType,
      logger: java.util.function.Consumer[String]
  )(implicit ctx: SourcePath.Context): Unit = {
    index.definition(module, Symbol(symbol)) match {
      case Some(defn) =>
        try {
          if (logger != null)
            logger.accept(s"Indexing javadoc / scaladoc of $symbol in $module")
          indexSymbolDefinition(module, defn, contentType)
          maybeCacheAlternative(module, defn, contentType)
          if (logger != null) logger.accept("Done indexing javadoc / scaladoc")
        } catch {
          case NonFatal(e) =>
            logger0.log(Level.SEVERE, defn.path.uri.toString, e)
            if (logger != null) {
              logger.accept(
                s"Error while indexing javadoc / scaladoc of $symbol in $module"
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
      module: GlobalSymbolIndex.Module,
      defn: SymbolDefinition,
      contentType: ContentType
  ) = {
    val defSymbol = defn.definitionSymbol.value
    val querySymbol = defn.querySymbol.value
    lazy val queryContent = (Content.from(querySymbol, contentType), module)

    if (
      defSymbol != querySymbol && cache.get(queryContent).forall {
        case EmptySymbolDocumentation | _: ProxySymbolDocumentation => true
        case _ => false
      }
    ) cache.put(queryContent, new ProxySymbolDocumentation(defSymbol))
  }

  private def indexSymbolDefinition(
      module: GlobalSymbolIndex.Module,
      defn: SymbolDefinition,
      contentType: ContentType
  )(implicit ctx: SourcePath.Context): Unit = {
    filenameToLanguage(defn.path.uri) match {
      case Language.JAVA =>
        JavadocIndexer.foreach(defn.path.toInput, contentType)(
          cacheSymbol(module, _, contentType)
        )
      case Language.SCALA =>
        for (dialect <- defn.dialectOpt)
          ScaladocIndexer.foreach(defn.path.toInput, dialect, contentType)(
            cacheSymbol(module, _, contentType)
          )
      case _ =>
    }
  }

  private class Deindexer(
      module: GlobalSymbolIndex.Module,
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
      } cache.remove((Content.from(sinfo.symbol, contentType), module))
    }
  }

  private class IndexSymbol(
      content: Content,
      symbol: String,
      module: GlobalSymbolIndex.Module,
      contentType: ContentType,
      logger: java.util.function.Consumer[String]
  )(implicit ctx: SourcePath.Context)
      extends Runnable {
    def run(): Unit =
      try {
        if (!cache.contains((content, module)))
          indexSymbol(module, symbol, contentType, logger)
        if (!cache.contains((content, module)))
          cache((content, module)) = EmptySymbolDocumentation
      } catch {
        case t: Throwable =>
          scribe.error(s"Error indexing docstrings of $symbol in $module", t)
      }
  }
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
