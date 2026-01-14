package tests

import scala.meta.Dialect
import scala.meta.internal.mtags
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.mtags.OnDemandSymbolIndex
import scala.meta.internal.mtags.SourcePath
import scala.meta.internal.mtags.SymbolDefinition
import scala.meta.io.AbsolutePath
import scala.meta.pc.reports.EmptyReportContext

/**
 * Symbol index that delegates all methods to an underlying implementation
 */
class DelegatingGlobalSymbolIndex(
    var underlying: OnDemandSymbolIndex =
      OnDemandSymbolIndex.empty()(new EmptyReportContext())
) extends GlobalSymbolIndex {

  def definitions(symbol: mtags.Symbol): List[SymbolDefinition] =
    underlying.definitions(symbol: mtags.Symbol)

  def definition(symbol: mtags.Symbol): Option[SymbolDefinition] = {
    underlying.definition(symbol)
  }
  def addSourceFile(
      file: SourcePath,
      dialect: Dialect
  )(implicit ctx: SourcePath.Context): Option[mtags.IndexingResult] = {
    underlying.addSourceFile(file, dialect)
  }
  def addSourceJar(
      jar: AbsolutePath,
      dialect: Dialect,
      reindex: Boolean = false
  ): List[mtags.IndexingResult] = {
    underlying.addSourceJar(jar, dialect, reindex)
  }

  def addSourceDirectory(
      dir: AbsolutePath,
      dialect: Dialect
  ): List[mtags.IndexingResult] = {
    underlying.addSourceDirectory(dir, dialect)
  }

  def findFileForToplevel(
      topLevelSymbol: mtags.Symbol
  )(implicit ctx: SourcePath.Context): List[(SourcePath, Dialect)] =
    underlying.findFileForToplevel(topLevelSymbol)
}
