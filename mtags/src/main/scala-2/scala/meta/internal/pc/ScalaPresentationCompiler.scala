package scala.meta.internal.pc

import java.io.File
import java.net.URI
import java.nio.file.Path
import java.util
import java.util.Optional
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.ScheduledExecutorService
import java.util.logging.Logger
import java.{util => ju}

import scala.collection.Seq
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.metals.PcQueryContext
import scala.meta.internal.metals.ReportLevel
import scala.meta.internal.mtags.BuildInfo
import scala.meta.internal.mtags.MtagsEnrichments._
import scala.meta.pc.AutoImportsResult
import scala.meta.pc.CodeActionId
import scala.meta.pc.CompileResult
import scala.meta.pc.CompletionItemPriority
import scala.meta.pc.DefinitionResult
import scala.meta.pc.DisplayableException
import scala.meta.pc.HoverSignature
import scala.meta.pc.InlayHintsParams
import scala.meta.pc.Node
import scala.meta.pc.OffsetParams
import scala.meta.pc.PresentationCompiler
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.RangeParams
import scala.meta.pc.ReferencesRequest
import scala.meta.pc.ReferencesResult
import scala.meta.pc.SymbolSearch
import scala.meta.pc.VirtualFileParams
import scala.meta.pc.reports.EmptyReportContext
import scala.meta.pc.reports.ReportContext
import scala.meta.pc.{PcSymbolInformation => IPcSymbolInformation}

import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionList
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DocumentHighlight
import org.eclipse.lsp4j.InlayHint
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.SelectionRange
import org.eclipse.lsp4j.SignatureHelp
import org.eclipse.lsp4j.TextEdit

class ScalaPresentationCompiler(
    var buildTargetIdentifier: String = "",
    var buildTargetName: Option[String] = None,
    var classpath: Seq[Path] = Nil,
    var options: List[String] = Nil,
    var search: SymbolSearch = EmptySymbolSearch,
    var ec: ExecutionContextExecutor = ExecutionContext.global,
    var sh: Option[ScheduledExecutorService] = None,
    var config: PresentationCompilerConfig = PresentationCompilerConfigImpl(),
    var folderPath: Option[Path] = None,
    var reportsLevel: ReportLevel = ReportLevel.Info,
    var completionItemPriority: CompletionItemPriority = (_: String) => 0,
    var optReportContext: Option[ReportContext] = None
) extends PresentationCompiler
    with HasCompilerAccess {
  implicit val reportContext: ReportContext =
    optReportContext.getOrElse(new EmptyReportContext())

  implicit val executionContext: ExecutionContextExecutor = ec

  val scalaVersion = BuildInfo.scalaCompilerVersion

  val logger: Logger =
    Logger.getLogger(classOf[ScalaPresentationCompiler].getName)

  override def withBuildTargetName(
      buildTargetName: String
  ): this.type = {
    this.buildTargetName = Some(buildTargetName)
    this
  }

  override def withReportsLoggerLevel(level: String): this.type = {
    this.reportsLevel = ReportLevel.fromString(level)
    this
  }

  override def withSearch(search: SymbolSearch): this.type = {
    this.search = search
    this
  }

  override def withWorkspace(workspace: Path): this.type = {
    this.folderPath = Some(workspace)
    this
  }

  override def withExecutorService(
      executorService: ExecutorService
  ): this.type = {
    this.ec = ExecutionContext.fromExecutorService(executorService)
    this
  }

  override def withScheduledExecutorService(
      sh: ScheduledExecutorService
  ): this.type = {
    this.sh = Some(sh)
    this
  }

  override def withConfiguration(
      config: PresentationCompilerConfig
  ): this.type = {
    this.config = config
    this
  }

  override def withCompletionItemPriority(
      priority: CompletionItemPriority
  ): this.type = {
    this.completionItemPriority = priority
    this
  }

  override def withReportContext(
      reportContext: ReportContext
  ): this.type = {
    this.optReportContext = Some(reportContext)
    this
  }

  override def supportedCodeActions(): util.List[String] = List(
    CodeActionId.ConvertToNamedArguments,
    CodeActionId.ImplementAbstractMembers,
    CodeActionId.ExtractMethod,
    CodeActionId.InlineValue,
    CodeActionId.InsertInferredType,
    CodeActionId.InsertInferredMethod
  ).asJava

  def additionalReportData(): String =
    s"""|Scala version: $scalaVersion
        |Classpath:
        |${classpath
         .map(path => s"$path [${if (path.exists) "exists" else "missing"} ]")
         .mkString(", ")}
        |Options:
        |${options.mkString(" ")}
        |""".stripMargin

  val compilerAccess: ScalaCompilerAccess =
    new ScalaCompilerAccess(
      config,
      sh,
      () => new ScalaCompilerWrapper(newCompiler())
    )(ec)

  override def shutdown(): Unit = {
    compilerAccess.shutdown()
  }

  def restart(): Unit = {
    compilerAccess.shutdownCurrentCompiler()
  }

  def isLoaded(): Boolean = compilerAccess.isLoaded()

  override def newInstance(
      buildTargetIdentifier: String,
      classpath: util.List[Path],
      options: util.List[String]
  ): this.type = {
    this.buildTargetIdentifier = buildTargetIdentifier
    this.classpath = classpath.asScala
    this.options = options.asScala.toList
    this
  }

  override def didChange(
      params: VirtualFileParams
  ): CompletableFuture[ju.List[Diagnostic]] = {
    CompletableFuture.completedFuture(Nil.asJava)
  }

  def didClose(uri: URI): Unit = {
    compilerAccess.withNonInterruptableCompiler(
      (),
      EmptyCancelToken
    ) { pc =>
      pc.compiler().richCompilationCache.remove(uri.toString())
    }(emptyQueryContext)
  }

  override def semanticTokens(
      params: VirtualFileParams
  ): CompletableFuture[ju.List[Node]] = {
    val empty: ju.List[Node] = new ju.ArrayList[Node]()
    compilerAccess.withInterruptableCompiler(
      empty,
      params.token,
      "semanticTokens",
      params.uri.toASCIIString
    ) { pc =>
      new PcSemanticTokensProvider(
        pc.compiler(params),
        params
      ).provide().asJava
    }(params.toQueryContext)
  }

  override def inlayHints(
      params: InlayHintsParams
  ): CompletableFuture[ju.List[InlayHint]] = {
    val empty: ju.List[InlayHint] = new ju.ArrayList[InlayHint]()
    implicit val queryInfo: PcQueryContext = params.toQueryContext
    compilerAccess.withInterruptableCompiler(
      empty,
      params.token,
      "syntheticDecorations",
      params.uri.toASCIIString
    ) { pc =>
      new PcInlayHintsProvider(
        pc.compiler(),
        params
      ).provide().asJava
    }
  }

  override def complete(
      params: OffsetParams
  ): CompletableFuture[CompletionList] = {
    implicit val queryInfo: PcQueryContext = params.toQueryContext
    compilerAccess.withInterruptableCompiler(
      EmptyCompletionList(),
      params.token,
      "complete",
      params.uri.toASCIIString
    ) { pc =>
      new CompletionProvider(pc.compiler(params), params)
        .completions()
    }
  }

  override def codeAction[T](
      params: OffsetParams,
      codeActionId: String,
      codeActionPayload: Optional[T]
  ): CompletableFuture[util.List[TextEdit]] = {
    (codeActionId, codeActionPayload.asScala) match {
      case (
            CodeActionId.ConvertToNamedArguments,
            Some(argIndices: ju.List[_])
          ) =>
        val payload = argIndices.asScala.collect { case i: Integer =>
          i.toInt
        }.toSet
        convertToNamedArguments(params, payload)
      case (CodeActionId.ImplementAbstractMembers, _) =>
        implementAbstractMembers(params)
      case (CodeActionId.InsertInferredType, _) =>
        insertInferredType(params)
      case (CodeActionId.InlineValue, _) =>
        inlineValue(params)
      case (CodeActionId.InsertInferredMethod, _) =>
        insertInferredMethod(params)
      case (CodeActionId.ExtractMethod, Some(extractionPos: OffsetParams)) =>
        params match {
          case range: RangeParams =>
            extractMethod(range, extractionPos)
          case _ =>
            CompletableFuture.failedFuture(
              new IllegalArgumentException(s"Expected range parameters")
            )
        }
      case (id, _) =>
        CompletableFuture.failedFuture(
          new IllegalArgumentException(s"Unsupported action id $id")
        )
    }
  }

  override def implementAbstractMembers(
      params: OffsetParams
  ): CompletableFuture[ju.List[TextEdit]] = {
    val empty: ju.List[TextEdit] = new ju.ArrayList[TextEdit]()
    implicit val queryInfo = params.toQueryContext
    compilerAccess.withInterruptableCompiler(
      empty,
      params.token,
      "implementAbstractMembers",
      params.uri.toASCIIString
    ) { pc =>
      new CompletionProvider(pc.compiler(params), params).implementAll()
    }
  }

  override def insertInferredType(
      params: OffsetParams
  ): CompletableFuture[ju.List[TextEdit]] = {
    val empty: ju.List[TextEdit] = new ju.ArrayList[TextEdit]()
    implicit val queryInfo: PcQueryContext = params.toQueryContext
    compilerAccess.withInterruptableCompiler(
      empty,
      params.token,
      "insertInferredType",
      params.uri.toASCIIString
    ) { pc =>
      new InferredTypeProvider(pc.compiler(params), params)
        .inferredTypeEdits()
        .asJava
    }
  }

  def insertInferredMethod(
      params: OffsetParams
  ): CompletableFuture[ju.List[TextEdit]] = {
    val empty: Either[String, List[TextEdit]] = Left(
      "Could not infer method, please report an issue in github.com/scalameta/metals"
    )
    implicit val queryInfo: PcQueryContext = params.toQueryContext
    compilerAccess
      .withInterruptableCompiler(
        empty,
        params.token
      ) { pc =>
        new InferredMethodProvider(pc.compiler(), params)
          .inferredMethodEdits()
      }
      .thenApply {
        case Right(edits: List[TextEdit]) => edits.asJava
        case Left(error: String) => throw new DisplayableException(error)
      }
  }

  override def inlineValue(
      params: OffsetParams
  ): CompletableFuture[ju.List[TextEdit]] = {
    val empty: Either[String, List[TextEdit]] = Right(List())
    (compilerAccess
      .withInterruptableCompiler(
        empty,
        params.token,
        "inlineValue",
        params.uri.toASCIIString
      ) { pc =>
        new PcInlineValueProviderImpl(
          pc.compiler(params),
          params
        ).getInlineTextEdits()
      }(params.toQueryContext))
      .thenApply {
        case Right(edits: List[TextEdit]) => edits.asJava
        case Left(error: String) => throw new DisplayableException(error)
      }
  }

  override def extractMethod(
      range: RangeParams,
      extractionPos: OffsetParams
  ): CompletableFuture[ju.List[TextEdit]] = {
    val empty: ju.List[TextEdit] = new ju.ArrayList[TextEdit]()
    implicit val queryInfo: PcQueryContext = range.toQueryContext
    compilerAccess.withInterruptableCompiler(
      empty,
      range.token,
      s"extractMethod(${range.uri})"
    ) { pc =>
      new ExtractMethodProvider(
        pc.compiler(range),
        range,
        extractionPos
      ).extractMethod.asJava
    }
  }

  override def convertToNamedArguments(
      params: OffsetParams,
      argIndices: ju.List[Integer]
  ): CompletableFuture[ju.List[TextEdit]] = {
    convertToNamedArguments(
      params,
      argIndices.asScala.map(_.toInt).toSet
    )
  }

  def convertToNamedArguments(
      params: OffsetParams,
      argIndices: Set[Int]
  ): CompletableFuture[ju.List[TextEdit]] = {
    val empty: Either[String, List[TextEdit]] = Right(List())
    (compilerAccess
      .withInterruptableCompiler(
        empty,
        params.token,
        "convertToNamedArguments",
        params.uri.toASCIIString
      ) { pc =>
        new ConvertToNamedArgumentsProvider(
          pc.compiler(params),
          params,
          argIndices
        ).convertToNamedArguments
      }(params.toQueryContext))
      .thenApply {
        case Left(error: String) => throw new DisplayableException(error)
        case Right(edits: List[TextEdit]) => edits.asJava
      }
  }

  override def autoImports(
      name: String,
      params: OffsetParams,
      isExtension: java.lang.Boolean // ignore, because Scala2 doesn't support extension method
  ): CompletableFuture[ju.List[AutoImportsResult]] = {
    implicit val queryInfo: PcQueryContext = params.toQueryContext
    compilerAccess.withInterruptableCompiler(
      List.empty[AutoImportsResult].asJava,
      params.token,
      "autoImports",
      params.uri.toASCIIString
    ) { pc =>
      new AutoImportsProvider(pc.compiler(params), name, params)
        .autoImports()
        .asJava
    }
  }

  override def getTasty(
      targetUri: URI,
      isHttpEnabled: Boolean
  ): CompletableFuture[String] =
    CompletableFuture.completedFuture("")

  // NOTE(olafur): hover and signature help use a "shared" compiler instance because
  // we don't typecheck any sources, we only poke into the symbol table.
  // If we used a shared compiler then we risk hitting `Thread.interrupt`,
  // which can close open `*-sources.jar` files containing Scaladoc/Javadoc strings.
  override def completionItemResolve(
      item: CompletionItem,
      symbol: String
  ): CompletableFuture[CompletionItem] =
    CompletableFuture.completedFuture {
      compilerAccess.withSharedCompiler(item) { pc =>
        new CompletionItemResolver(pc.compiler()).resolve(item, symbol)
      }(emptyQueryContext)
    }

  override def signatureHelp(
      params: OffsetParams
  ): CompletableFuture[SignatureHelp] = {
    implicit val queryInfo: PcQueryContext = params.toQueryContext
    compilerAccess.withNonInterruptableCompiler(
      new SignatureHelp(),
      params.token,
      "signatureHelp",
      params.uri.toASCIIString
    ) { pc =>
      new SignatureHelpProvider(pc.compiler(params))
        .signatureHelp(params)
    }
  }

  override def prepareRename(
      params: OffsetParams
  ): CompletableFuture[ju.Optional[Range]] =
    compilerAccess.withNonInterruptableCompiler(
      Optional.empty[Range](),
      params.token,
      "prepareRename",
      params.uri.toASCIIString
    ) { pc =>
      new PcRenameProvider(pc.compiler(params), params, None)
        .prepareRename()
        .asJava
    }(params.toQueryContext)

  override def rename(
      params: OffsetParams,
      name: String
  ): CompletableFuture[ju.List[TextEdit]] =
    compilerAccess.withNonInterruptableCompiler(
      List[TextEdit]().asJava,
      params.token,
      "rename",
      params.uri.toASCIIString
    ) { pc =>
      new PcRenameProvider(
        pc.compiler(params),
        params,
        Some(name)
      ).rename().asJava
    }(params.toQueryContext)

  override def compile(
      params: VirtualFileParams
  ): CompletableFuture[CompileResult] =
    compilerAccess.withNonInterruptableCompiler[CompileResult](
      CompileProvider.Result(Nil, ""),
      params.token,
      "compile",
      params.uri.toASCIIString
    ) { pc =>
      val compiler = pc.compiler()
      new CompileProvider(compiler, params).compile()
    }(params.toQueryContext)

  override def hover(
      params: OffsetParams
  ): CompletableFuture[Optional[HoverSignature]] = {
    implicit val queryInfo: PcQueryContext = params.toQueryContext
    compilerAccess.withNonInterruptableCompiler(
      Optional.empty[HoverSignature](),
      params.token,
      "hover",
      params.uri.toASCIIString
    ) { pc =>
      Optional.ofNullable(
        new HoverProvider(
          pc.compiler(params),
          params,
          config.hoverContentType()
        )
          .hover()
          .orNull
      )
    }
  }

  def definition(params: OffsetParams): CompletableFuture[DefinitionResult] = {
    compilerAccess.withNonInterruptableCompiler(
      DefinitionResultImpl.empty,
      params.token,
      "definition",
      params.uri.toASCIIString
    ) { pc =>
      new PcDefinitionProvider(pc.compiler(params), params)
        .definition()
    }(params.toQueryContext)
  }

  override def info(
      symbol: String
  ): CompletableFuture[Optional[IPcSymbolInformation]] = {
    compilerAccess.withNonInterruptableCompiler[Optional[IPcSymbolInformation]](
      Optional.empty(),
      EmptyCancelToken
    ) { pc =>
      val result: Option[IPcSymbolInformation] =
        pc.compiler().info(symbol).map(_.asJava)
      result.asJava
    }(emptyQueryContext)
  }

  override def typeDefinition(
      params: OffsetParams
  ): CompletableFuture[DefinitionResult] = {
    compilerAccess.withNonInterruptableCompiler(
      DefinitionResultImpl.empty,
      params.token,
      "typeDefinition",
      params.uri.toASCIIString
    ) { pc =>
      new PcDefinitionProvider(pc.compiler(params), params)
        .typeDefinition()
    }(params.toQueryContext)
  }

  override def documentHighlight(
      params: OffsetParams
  ): CompletableFuture[util.List[DocumentHighlight]] =
    compilerAccess.withInterruptableCompiler(
      List.empty[DocumentHighlight].asJava,
      params.token(),
      "documentHighlight",
      params.uri.toASCIIString
    ) { pc =>
      new PcDocumentHighlightProvider(pc.compiler(params), params)
        .highlights()
        .asJava
    }(params.toQueryContext)

  override def references(
      params: ReferencesRequest
  ): CompletableFuture[ju.List[ReferencesResult]] = {
    compilerAccess.withInterruptableCompiler(
      List.empty[ReferencesResult].asJava,
      params.file.token()
    ) { pc =>
      val res: List[ReferencesResult] =
        PcReferencesProvider(pc.compiler(), params).references()
      res.asJava
    }(params.file.toQueryContext)
  }

  override def semanticdbTextDocument(
      fileUri: URI,
      code: String
  ): CompletableFuture[Array[Byte]] = {
    val virtualFile = CompilerVirtualFileParams(fileUri, code)
    semanticdbTextDocument(virtualFile)
  }

  override def semanticdbTextDocument(
      virtualFile: VirtualFileParams
  ): CompletableFuture[Array[Byte]] = {
    compilerAccess.withInterruptableCompiler(
      Array.emptyByteArray,
      EmptyCancelToken,
      "semanticdbTextDocument",
      virtualFile.uri().toASCIIString
    ) { pc =>
      new SemanticdbTextDocumentProvider(
        pc.compiler(virtualFile),
        config.semanticdbCompilerOptions().asScala.toList
      )
        .textDocument(virtualFile.uri(), virtualFile.text())
        .toByteArray
    }(virtualFile.toQueryContext)
  }

  override def selectionRange(
      params: ju.List[OffsetParams]
  ): CompletableFuture[ju.List[SelectionRange]] = {
    CompletableFuture.completedFuture {
      compilerAccess.withSharedCompiler(
        List.empty[SelectionRange].asJava
      ) { pc =>
        new SelectionRangeProvider(pc.compiler(), params)
          .selectionRange()
          .asJava
      }(
        params.asScala.headOption
          .map(_.toQueryContext)
          .getOrElse(emptyQueryContext)
      )
    }
  }

  override def buildTargetId(): String = buildTargetIdentifier

  def newCompiler(): MetalsGlobal = {
    val classpath = this.classpath.mkString(File.pathSeparator)
    val vd = new VirtualDirectory("(memory)", None)
    val settings = new Settings
    settings.Ymacroexpand.value = "discard"
    settings.outputDirs.setSingleOutput(vd)
    settings.classpath.value = classpath
    settings.YpresentationAnyThread.value = true
    if (
      !BuildInfo.scalaCompilerVersion.startsWith("2.11") &&
      BuildInfo.scalaCompilerVersion != "2.12.4"
    ) {
      settings.processArguments(
        List("-Ycache-plugin-class-loader:last-modified"),
        processAll = true
      )
    }
    if (classpath.isEmpty) {
      settings.usejavacp.value = true
    }
    val (isSuccess, unprocessed) =
      settings.processArguments(options, processAll = true)
    if (unprocessed.nonEmpty || !isSuccess) {
      logger.warning(s"Unknown compiler options: ${unprocessed.mkString(", ")}")
    }
    new MetalsGlobal(
      settings,
      new StoreReporter,
      search,
      buildTargetIdentifier,
      config,
      folderPath,
      completionItemPriority
    )
  }

  // ================
  // Internal methods
  // ================

  override def diagnosticsForDebuggingPurposes(): util.List[String] = {
    compilerAccess.reporter.infos.iterator
      .map { info =>
        new StringBuilder()
          .append(info.pos.source.file.path)
          .append(":")
          .append(info.pos.column)
          .append(" ")
          .append(info.msg)
          .append("\n")
          .append(info.pos.lineContent)
          .append("\n")
          .append(info.pos.lineCaret)
          .toString
      }
      .filterNot(_.contains("_CURSOR_"))
      .toList
      .asJava
  }

  implicit class XtensionParams(params: VirtualFileParams) {
    def toQueryContext: PcQueryContext =
      PcQueryContext(Some(params), additionalReportData)

  }

  def emptyQueryContext: PcQueryContext =
    PcQueryContext(None, additionalReportData)

}
