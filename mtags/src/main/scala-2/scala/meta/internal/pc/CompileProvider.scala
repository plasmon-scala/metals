package scala.meta.internal.pc

import scala.meta.pc.CompileResult
import scala.meta.pc.VirtualFileParams
import java.{util => ju}
import scala.jdk.CollectionConverters._

class CompileProvider(val compiler: MetalsGlobal, params: VirtualFileParams) {
  import compiler._

  def compile(): CompileProvider.Result = {
    val unit = addCompilationUnit(
      code = params.text(),
      filename = params.uri().toString(),
      cursor = None
    )
    metalsTypeCheck(unit)

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
      .toList

    CompileProvider.Result(diagnostics, unit.body.toString)
  }

}

object CompileProvider {
  final case class Result(
      scalaDiagnostics: List[String],
      fullTree: String
  ) extends CompileResult {
    def diagnostics(): ju.List[String] = scalaDiagnostics.asJava
  }
}
