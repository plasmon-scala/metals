package scala.meta.internal.pc

import java.lang
import java.net.URI
import java.nio.file.Path
import java.util
import java.util.Optional
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.ScheduledExecutorService

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import scala.jdk.CollectionConverters._

import scala.compat.java8.FutureConverters
import scala.meta.pc.AutoImportsResult
import scala.meta.pc.DefinitionResult
import scala.meta.pc.HoverSignature
import scala.meta.pc.InlayHintsParams
import scala.meta.pc.OffsetParams
import scala.meta.pc.PresentationCompiler
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.RangeParams
import scala.meta.pc.ReferencesRequest
import scala.meta.pc.ReferencesResult
import scala.meta.pc.SymbolSearch
import scala.meta.pc.VirtualFileParams
import scala.meta.pc.CompileResult

import org.eclipse.lsp4j
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionList
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DocumentHighlight
import org.eclipse.lsp4j.SelectionRange
import org.eclipse.lsp4j.SignatureHelp
import org.eclipse.lsp4j.TextEdit
import scala.meta.pc.ContentType
import javax.tools.JavaFileManager
import scala.concurrent.Future

case class JavaPresentationCompiler(
    javaFileManager: () => JavaFileManager,
    logger: java.util.function.Consumer[String],
    moduleString: String,
    ec: ExecutionContextExecutor,
    classpath: Seq[Path] = Nil,
    search: SymbolSearch = EmptySymbolSearch,
    config: PresentationCompilerConfig =
      PresentationCompilerConfigImpl(hoverContentType = ContentType.MARKDOWN)
) extends PresentationCompiler {

  private lazy val javaCompiler = {
    logger.accept(s"Creating new Java presentation compiler for $moduleString")
    logger.accept("Class path:")
    for (p <- classpath)
      logger.accept(s"  $p")
    new JavaMetalsGlobal(javaFileManager(), search, config, classpath)
  }

  private def run[T](f: => T): CompletableFuture[T] =
    FutureConverters.toJava(Future(f)(ec)).toCompletableFuture

  override def complete(
      params: OffsetParams
  ): CompletableFuture[CompletionList] =
    run {
      new JavaCompletionProvider(
        javaCompiler,
        params,
        config.isCompletionSnippetsEnabled
      ).completions()
    }

  override def completionItemResolve(
      item: CompletionItem,
      symbol: String
  ): CompletableFuture[CompletionItem] =
    CompletableFuture.completedFuture(item)

  override def signatureHelp(
      params: OffsetParams
  ): CompletableFuture[SignatureHelp] =
    CompletableFuture.completedFuture(new SignatureHelp)

  override def hover(
      params: OffsetParams
  ): CompletableFuture[Optional[HoverSignature]] =
    run {
      Optional.ofNullable(
        new JavaHoverProvider(
          javaCompiler,
          params,
          config.hoverContentType(),
          logger
        )
          .hover(moduleString)
          .orNull
      )
    }

  override def compile(
      params: VirtualFileParams
  ): CompletableFuture[CompileResult] = {
    ???
  }

  override def rename(
      params: OffsetParams,
      name: String
  ): CompletableFuture[util.List[TextEdit]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def definition(
      params: OffsetParams
  ): CompletableFuture[DefinitionResult] =
    CompletableFuture.completedFuture(DefinitionResultImpl.empty)

  override def typeDefinition(
      params: OffsetParams
  ): CompletableFuture[DefinitionResult] =
    CompletableFuture.completedFuture(DefinitionResultImpl.empty)

  override def documentHighlight(
      params: OffsetParams
  ): CompletableFuture[util.List[DocumentHighlight]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def references(
      params: ReferencesRequest
  ): CompletableFuture[util.List[ReferencesResult]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def getTasty(
      targetUri: URI,
      isHttpEnabled: Boolean
  ): CompletableFuture[String] = CompletableFuture.completedFuture("")

  override def autoImports(
      name: String,
      params: OffsetParams,
      isExtension: lang.Boolean
  ): CompletableFuture[util.List[AutoImportsResult]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def implementAbstractMembers(
      params: OffsetParams
  ): CompletableFuture[util.List[TextEdit]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def insertInferredType(
      params: OffsetParams
  ): CompletableFuture[util.List[TextEdit]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def extractMethod(
      range: RangeParams,
      extractionPos: OffsetParams
  ): CompletableFuture[util.List[TextEdit]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def convertToNamedArguments(
      params: OffsetParams,
      argIndices: util.List[Integer]
  ): CompletableFuture[util.List[TextEdit]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def inlayHints(
      params: InlayHintsParams
  ): CompletableFuture[util.List[lsp4j.InlayHint]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def didChange(
      params: VirtualFileParams
  ): CompletableFuture[util.List[Diagnostic]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def didClose(uri: URI): Unit = ()

  override def semanticdbTextDocument(
      filename: URI,
      code: String
  ): CompletableFuture[Array[Byte]] =
    CompletableFuture.completedFuture(Array.emptyByteArray)

  override def selectionRange(
      params: util.List[OffsetParams]
  ): CompletableFuture[util.List[SelectionRange]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def shutdown(): Unit = ()

  override def restart(): Unit = ()

  override def withSearch(search: SymbolSearch): PresentationCompiler =
    copy(search = search)

  override def withExecutorService(
      executorService: ExecutorService
  ): PresentationCompiler =
    copy(ec = ExecutionContext.fromExecutorService(executorService))

  override def withScheduledExecutorService(
      scheduledExecutorService: ScheduledExecutorService
  ): PresentationCompiler =
    this

  override def withConfiguration(
      config: PresentationCompilerConfig
  ): PresentationCompiler = copy(config = config)

  override def withWorkspace(workspace: Path): PresentationCompiler =
    this

  override def newInstance(
      moduleString: String,
      classpath: util.List[Path],
      options: util.List[String]
  ): PresentationCompiler =
    copy(
      moduleString = moduleString,
      classpath = classpath.asScala.toSeq
    )

  override def diagnosticsForDebuggingPurposes(): util.List[String] = Nil.asJava

  override def isLoaded: Boolean = true

  override def scalaVersion(): String = "java"

  override def prepareRename(
      params: OffsetParams
  ): CompletableFuture[Optional[lsp4j.Range]] =
    CompletableFuture.completedFuture(Optional.empty())
}
