package scala.meta.internal.jpc

import java.util.concurrent.ScheduledExecutorService

import scala.concurrent.ExecutionContextExecutor

import scala.meta.internal.pc.CompilerAccess
import scala.meta.pc.DisplayableException
import scala.meta.pc.PresentationCompilerConfig

import org.slf4j.Logger

class JavaPresentationCompilerAccess(
    logger: Logger,
    config: PresentationCompilerConfig,
    sh: Option[ScheduledExecutorService],
    newCompiler: () => JavaMetalsCompilerWrapper,
    userLogger: java.util.function.Consumer[String],
    id: String = ""
)(implicit ec: ExecutionContextExecutor)
    extends CompilerAccess[Unit, JavaMetalsCompiler](
      logger,
      config,
      sh,
      newCompiler,
      shouldResetJobQueue = false,
      userLogger,
      id
    ) {
  override protected def newReporter: Unit = ()
  override protected def handleSharedCompilerException(
      t: Throwable
  ): Option[String] = t match {
    case _: DisplayableException => throw t
    case _ => Some(s"an error in the Java compiler: ${t}")
  }

  override protected def ignoreException(t: Throwable): Boolean = false

}
