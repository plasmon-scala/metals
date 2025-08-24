package scala.meta.internal.pc

import java.util.Optional

import scala.reflect.internal.util.Position
import scala.reflect.internal.{Flags => gf}

import scala.meta.internal.metals.PcQueryContext
import scala.meta.internal.metals.Report
import scala.meta.internal.mtags.MtagsEnrichments._
import scala.meta.pc.ContentType
import scala.meta.pc.HoverSignature
import scala.meta.pc.OffsetParams
import scala.meta.pc.RangeParams
import scala.meta.pc.reports.ReportContext
import scala.meta.internal.mtags.GlobalSymbolIndex

class HoverProvider(
    val compiler: MetalsGlobal,
    params: OffsetParams,
    contentType: ContentType
)(implicit reportContext: ReportContext, queryInfo: PcQueryContext) {
  import compiler._

  def hover(module: GlobalSymbolIndex.Module): Option[HoverSignature] =
    params match {
      case range: RangeParams =>
        val paramsOpt = range.trimWhitespaceInRange
        paramsOpt.flatMap(hoverOffset(module, _))
      case _ if params.isWhitespace && params.prevIsWhitespaceOrDelimeter =>
        None
      case _ => hoverOffset(module, params)
    }

  def hoverOffset(
      module: GlobalSymbolIndex.Module,
      params: OffsetParams
  ): Option[HoverSignature] = {
    val unit = addCompilationUnit(
      code = params.text(),
      filename = params.uri().toString(),
      cursor = None
    )
    val (pos, tree) = params match {
      case params: RangeParams =>
        val pos = Position.range(
          unit.source,
          params.offset(),
          params.offset(),
          params.endOffset()
        )
        val tree = typedHoverTreeAt(pos, unit)
        (pos, tree)
      case params: OffsetParams =>
        val pos = unit.position(params.offset())
        val tree = typedHoverTreeAt(pos, unit)
        (pos, tree)
    }

    def report = {
      val hasErroneousType =
        if (tree.tpe != null) tree.tpe.isErroneous
        else "type null"
      val fileName = params.uri()
      val posId =
        if (tree.pos.isDefined) tree.pos.start
        else pos.start

      val diagnostics = compiler.reporter
        .asInstanceOf[scala.tools.nsc.reporters.StoreReporter]
        .infos
        .iterator
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
            .append("\n")
            .toString
        }
        .filterNot(_.contains("_CURSOR_"))
        .mkString

      Report(
        "empty-hover-scala2",
        s"""|pos: ${pos.toLsp}
            |
            |diagnostics: $diagnostics
            |
            |is error: $hasErroneousType
            |symbol: ${tree.symbol}
            |tpe: ${tree.tpe}
            |
            |tree:
            |```scala
            |$tree
            |```
            |
            |full tree:
            |```scala
            |${unit.body}
            |```
            |""".stripMargin,
        s"empty hover in $fileName",
        id = Optional.of(s"$fileName::$posId"),
        path = Optional.of(fileName)
      )
    }

    tree match {
      case i @ Import(_, _) =>
        for {
          member <- i.selector(pos)
          hover <- toHover(
            module,
            member,
            member.keyString,
            member.info,
            member.info,
            pos,
            pos,
            Some(report)
          )
        } yield hover
      case _: Select | _: Apply | _: TypeApply | _: Ident =>
        val expanded = expandRangeToEnclosingApply(pos)
        if (
          expanded != null &&
          expanded.tpe != null &&
          tree.symbol != null &&
          expanded.symbol != null
        ) {
          val symbol =
            if (expanded.symbol.isConstructor) expanded.symbol
            else tree.symbol
          toHover(
            module,
            symbol,
            symbol.keyString,
            seenFromType(tree, symbol),
            expanded.tpe,
            pos,
            expanded.pos,
            Some(report)
          )
        } else {
          val res = for {
            sym <- Option(tree.symbol)
            tpe <- Option(tree.tpe)
            seenFrom = seenFromType(tree, sym)
            hover <- toHover(
              module,
              sym,
              sym.keyString,
              seenFrom,
              tpe,
              pos,
              tree.pos,
              Some(report)
            )
          } yield hover
          if (res.isEmpty)
            reportContext.unsanitized().create(() => report, true)
          res
        }
      case UnApply(fun, _) if fun.symbol != null =>
        toHover(
          module,
          fun.symbol,
          fun.symbol.keyString,
          seenFromType(tree, fun.symbol),
          tree.tpe,
          pos,
          pos,
          Some(report)
        )
      // Def, val or val definition, example `val x: Int = 1`
      // Matches only if the cursor is over the definition name.
      case v: ValOrDefDef
          if (v.namePosition
            .includes(pos) || pos.includes(
            v.namePosition
          )) && v.symbol != null =>
        val symbol = (v.symbol.getter: Symbol) match {
          case NoSymbol => v.symbol
          case getter => getter
        }
        toHover(
          module,
          symbol,
          v.symbol.keyString,
          symbol.info,
          symbol.info,
          pos,
          v.pos,
          Some(report)
        )
      // Bound variable in a pattern match, example `head` in `case head :: tail =>`
      case _: Bind =>
        val symbol = tree.symbol
        toHover(
          module = module,
          symbol = symbol,
          keyword = "",
          seenFrom = symbol.info,
          tpe = symbol.info,
          pos = pos,
          range = pos,
          Some(report)
        )
      case _: Literal if params.isInstanceOf[RangeParams] =>
        val symbol = tree.symbol
        toHover(
          module = module,
          symbol = symbol,
          keyword = "",
          seenFrom = null,
          tpe = tree.tpe,
          pos = pos,
          range = pos,
          Some(report)
        )
      // Class, object, type definitions, matches only if the cursor is over the definition name.
      case t: MemberDef
          if (t.namePosition
            .includes(pos) || pos.includes(
            t.namePosition
          )) && t.symbol != null =>
        val symbol = tree.symbol
        val tpe = seenFromType(tree, symbol)
        toHover(
          module = module,
          symbol = symbol,
          keyword = symbol.keyString,
          seenFrom = tpe,
          tpe = tpe,
          pos = pos,
          range = t.namePosition,
          Some(report)
        )
      case _ =>
        reportContext.unsanitized.create(() => report, true)
        // Don't show hover for non-identifiers.
        None
    }
  }

  def toHover(
      module: GlobalSymbolIndex.Module,
      symbol: Symbol,
      pos: Position
  ): Option[HoverSignature] = {
    toHover(
      module,
      symbol,
      symbol.keyString,
      symbol.info,
      symbol.info,
      pos,
      pos
    )
  }

  def toHover(
      module: GlobalSymbolIndex.Module,
      symbol: Symbol,
      keyword: String,
      seenFrom: Type,
      tpe: Type,
      pos: Position,
      range: Position,
      report: => Option[Report] = None
  ): Option[HoverSignature] = {

    def docstring =
      if (metalsConfig.isHoverDocumentationEnabled) {
        symbolDocumentation(module, symbol, contentType)
          .filter(docs => !docs.docstring().isEmpty())
          .orElse(symbolDocumentation(module, symbol.companion, contentType))
          .fold("")(_.docstring())
      } else {
        ""
      }

    val result =
      if (tpe == null || tpe.isErroneous || tpe == NoType) None
      else if (
        pos.start != pos.end && (symbol == null || symbol == NoSymbol || symbol.isErroneous)
      ) {
        val context = doLocateContext(pos)
        val re: scala.collection.Map[Symbol, Name] = renamedSymbols(context)
        val history = new ShortenedNames(
          lookupSymbol = name => context.lookupSymbol(name, _ => true) :: Nil,
          renames = re
        )
        val prettyType =
          metalsToLongString(tpe.widen.finalResultType.map(_.dealias), history)
        val lspRange = if (range.isRange) Some(range.toLsp) else None
        Some(
          new ScalaHover(
            expressionType = Some(prettyType),
            range = lspRange,
            contextInfo = history.getUsedRenamesInfo(),
            contentType = contentType
          )
        )
      } else if (symbol == null || tpe.typeSymbol.isAnonymousClass) None
      else if (symbol.hasPackageFlag || symbol.hasModuleFlag) {
        Some(
          new ScalaHover(
            expressionType = Some(
              s"${symbol.javaClassSymbol.keyString} ${symbol.fullName}"
            ),
            contextInfo = Nil,
            docstring = if (symbol.hasModuleFlag) Some(docstring) else None,
            contentType = contentType
          )
        )
      } else {
        val context = doLocateContext(pos)
        val re: scala.collection.Map[Symbol, Name] = renamedSymbols(context)
        val history = new ShortenedNames(
          lookupSymbol = name => context.lookupSymbol(name, _ => true) :: Nil,
          renames = re
        )
        val symbolInfo =
          if (seenFrom.isErroneous) symbol.info
          else seenFrom
        val printer = new SignaturePrinter(
          symbol,
          history,
          symbolInfo.widen,
          moduleIfIncludeDocs = Some(module)
        )
        val name =
          if (symbol.isConstructor) "this"
          else Identifier.backtickWrap(symbol.name.decoded)
        val flags =
          List(symbolFlagString(symbol), keyword, name)
            .filterNot(_.isEmpty)
            .mkString(" ")
        val prettyType =
          metalsToLongString(tpe.widen.finalResultType.map(_.dealias), history)
        val macroSuffix =
          if (symbol.isMacro) " = macro"
          else ""
        val isClassLike =
          symbol.isJavaInterface || symbol.isTrait || symbol.isClass ||
            (symbol.isType && !symbol.isParameter) || symbol.hasPackageFlag || symbol.isModule
        val prettySignature =
          printer.defaultMethodSignature(
            flags,
            isClassLike = isClassLike
          ) + macroSuffix
        Some(
          ScalaHover(
            expressionType = Some(prettyType),
            symbolSignature = Some(prettySignature),
            docstring = Some(docstring),
            forceExpressionType =
              !isClassLike && (pos.start != pos.end || (!prettySignature
                .endsWith(prettyType) && !symbol.isType)),
            range = if (range.isRange) Some(range.toLsp) else None,
            contextInfo = history.getUsedRenamesInfo(),
            contentType = contentType
          )
        )
      }

    if (result.isEmpty)
      for (report0 <- report) {
        reportContext
          .unsanitized()
          .create(() => report0, /*ifVerbose = true*/ true)
        val content = report0.fullText(withIdAndSummary = true)
        compiler.userLogger.accept(content)
      }
    result
  }

  def symbolFlagString(sym: Symbol): String = {
    var mask = sym.flagMask
    // Strip case modifier off non-class symbols like synthetic apply/copy.
    if (sym.isCase && !sym.isClass) mask &= ~gf.CASE
    mask &= ~gf.OVERRIDE
    mask &= ~gf.FINAL
    mask &= ~gf.AccessFlags
    mask &= ~gf.JAVA_DEFAULTMETHOD

    // "abstract trait" feels odd, just "trait" is enough
    if ((sym.flags & gf.TRAIT) != 0) mask &= ~gf.ABSTRACT
    // "final" in "final case" is superfluous, just "case" is enough
    if ((sym.flags & gf.CASE) != 0 && (sym.flags & gf.FINAL) != 0)
      mask &= ~gf.FINAL

    // put "case" at the end, to avoid things like "case sealed abstract class"
    val isCase = (sym.flags & gf.CASE) != 0
    if (isCase) mask &= ~gf.CASE

    // calling flagBitsToString rather than flagString, to drop access stuff, that
    // can't be dropped with just a mask
    val str = sym.flagBitsToString(sym.flags & mask)
    if (isCase) Seq(str, "case").filter(_.nonEmpty).mkString(" ")
    else str
  }

  private def typedHoverTreeAt(
      pos: Position,
      unit: RichCompilationUnit
  ): Tree = {
    metalsTypeCheck(unit)
    val typedTree = locateTree(pos)
    typedTree match {
      case Import(qual, _) if qual.pos.includes(pos) =>
        qual.findSubtree(pos)
      case Apply(fun, args)
          if !fun.pos.includes(pos) &&
            !isForSynthetic(typedTree) =>
        // Looks like a named argument, try the arguments.
        val arg = args.collectFirst {
          case arg if treePos(arg).includes(pos) =>
            arg match {
              case Block(_, expr) if treePos(expr).includes(pos) =>
                // looks like a desugaring of named arguments in different order from definition-site.
                expr
              case a => a
            }
        }
        arg.getOrElse(typedTree)
      case t => t
    }
  }

  lazy val isForName: Set[Name] = Set[Name](
    nme.map,
    nme.withFilter,
    nme.flatMap,
    nme.foreach
  )
  def isForSynthetic(gtree: Tree): Boolean = {
    def isForComprehensionSyntheticName(select: Select): Boolean = {
      select.pos == select.qualifier.pos && isForName(select.name)
    }
    gtree match {
      case Apply(fun, List(_: Function)) => isForSynthetic(fun)
      case TypeApply(fun, _) => isForSynthetic(fun)
      case gtree: Select if isForComprehensionSyntheticName(gtree) => true
      case _ => false
    }
  }

}
