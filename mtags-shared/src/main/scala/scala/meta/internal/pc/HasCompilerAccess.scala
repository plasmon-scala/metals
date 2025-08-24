package scala.meta.internal.pc

trait HasCompilerAccess {
  def compilerAccess: CompilerAccess[_, _]
  def latestException(): Option[(Option[String], Throwable)]
}
