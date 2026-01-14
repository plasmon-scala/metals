package scala.meta.internal.mtags

import scala.meta.internal.{semanticdb => s}

final case class SymbolLocation(
    path: SourcePath,
    range: Option[s.Range]
)
