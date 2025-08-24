package scala.meta.internal.pc

trait HasCompilerAccess {
  def compilerAccess: CompilerAccess[_, _]
}
