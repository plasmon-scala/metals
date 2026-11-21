package scala.meta.internal.pc

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.nio.charset.StandardCharsets
import java.time.Duration
import java.util.UUID
import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.ExecutionContextExecutor
import scala.util.control.NonFatal

import scala.meta.internal.metals.PcQueryContext
import scala.meta.pc.CancelToken
import scala.meta.pc.PresentationCompilerConfig

import org.slf4j.Logger

/**
 * Manages the lifecycle and multi-threaded access to the presentation compiler.
 *
 * - automatically restarts the compiler on miscellaneous crashes.
 * - handles cancellation via `Thread.interrupt()` to stop the compiler during typechecking,
 *   for functions that support cancellation.
 */
abstract class CompilerAccess[+Reporter, Compiler](
    logger: Logger,
    config: PresentationCompilerConfig,
    sh: Option[ScheduledExecutorService],
    newCompiler: () => CompilerWrapper[Reporter, Compiler],
    shouldResetJobQueue: Boolean,
    userLogger: java.util.function.Consumer[String],
    id: String = ""
)(implicit ec: ExecutionContextExecutor) {

  var latestExceptionOpt: Option[(Option[String], Throwable)] = None

  val jobs: CompilerJobQueue = CompilerJobQueue(id, userLogger)
  private var __compiler: CompilerWrapper[_, Compiler] = _
  private def _compiler: CompilerWrapper[Reporter, Compiler] =
    __compiler.asInstanceOf[CompilerWrapper[Reporter, Compiler]]
  private var isShutdown: Boolean = false
  // Whether the current thread runs a job of the queue: jobs queued before a shutdown still run
  private val inQueuedJob = new ThreadLocal[java.lang.Boolean] {
    override def initialValue(): java.lang.Boolean = false
  }
  private def isEmpty: Boolean = _compiler == null
  private def isDefined: Boolean = !isEmpty
  private def loadCompiler(): CompilerWrapper[Reporter, Compiler] =
    synchronized {
      if (isShutdown && !inQueuedJob.get()) {
        throw new CancellationException()
      }
      if (_compiler == null) {
        __compiler = newCompiler()
      }
      _compiler.resetReporter()
      _compiler
    }

  protected def newReporter: Reporter

  def reporter: Reporter =
    if (isEmpty) newReporter
    else _compiler.reporterAccess.reporter

  def isLoaded(): Boolean = _compiler != null

  def shutdown(): Unit = {
    val compiler = synchronized {
      isShutdown = true
      detachCurrentCompiler()
    }
    stopCompiler(compiler)
    jobs.shutdown()
  }

  def shutdownCurrentCompiler(): Unit = {
    // Detach under the same lock as loadCompiler so a concurrent load cannot
    // obtain the compiler being stopped or be overwritten afterwards.
    val compiler = synchronized {
      detachCurrentCompiler()
    }
    stopCompiler(compiler)
  }

  private def detachCurrentCompiler(): CompilerWrapper[Reporter, Compiler] = {
    val current = _compiler
    __compiler = null
    current
  }

  private def stopCompiler(
      compiler: CompilerWrapper[_, Compiler]
  ): Unit = {
    if (compiler != null) {
      compiler.stop()
      sh.foreach { scheduler =>
        scheduler.schedule[Unit](
          () => {
            if (compiler.isAlive()) {
              compiler.stop()
            }
          },
          2,
          TimeUnit.SECONDS
        )
      }
    }
  }

  private var beforeAccess0 = Option.empty[(String, String, String) => Unit]
  private var afterAccess0 = Option.empty[(String, String, String) => Unit]
  def beforeAccess(f: (String, String, String) => Unit): Unit = {
    beforeAccess0 = Some(f)
  }
  def afterAccess(f: (String, String, String) => Unit): Unit = {
    afterAccess0 = Some(f)
  }

  def interrupt(): Boolean =
    _compiler.presentationCompilerThread match {
      case None => false
      case Some(pcThread) =>
        pcThread.interrupt()
        true
    }

  /**
   * Asynchronously execute a function on the compiler thread with `Thread.interrupt()` cancellation.
   */
  def withInterruptableCompiler[T](
      default: T,
      token: CancelToken,
      name: String = "",
      uri: String = "",
      customTimeout: Option[Duration] = None
  )(
      thunk: CompilerWrapper[Reporter, Compiler] => T
  )(implicit queryInfo: PcQueryContext): CompletableFuture[T] = {
    val isFinished = new AtomicBoolean(false)
    var queueThread = Option.empty[Thread]
    val result = onCompilerJobQueue(
      name,
      uri,
      () => {
        queueThread = Some(Thread.currentThread())
        try withSharedCompiler(default, uri)(thunk)
        finally isFinished.set(true)
      },
      token,
      customTimeout
    )

    // Interrupt the queue thread
    token.onCancel.whenCompleteAsync(
      (isCancelled, _) => {
        queueThread.foreach { thread =>
          if (
            isCancelled &&
            isFinished.compareAndSet(false, true) &&
            isDefined
          ) {
            _compiler.presentationCompilerThread match {
              case None => // don't interrupt if we don't have separate thread
              case Some(pcThread) =>
                pcThread.interrupt()
                if (thread != pcThread) thread.interrupt()
            }
          }
        }
      },
      ec
    )
    result
  }

  /**
   * Asynchronously execute a function on the compiler thread without `Thread.interrupt()` cancellation.
   *
   * Note that the function is still cancellable.
   */
  def withNonInterruptableCompiler[T](
      default: T,
      token: CancelToken,
      name: String = "",
      uri: String = ""
  )(
      thunk: CompilerWrapper[Reporter, Compiler] => T
  )(implicit queryInfo: PcQueryContext): CompletableFuture[T] = {
    onCompilerJobQueue(
      name,
      uri,
      () => withSharedCompiler(default, uri)(thunk),
      token
    )
  }

  /**
   * Execute a function on the current thread without cancellation support.
   *
   * May potentially run in parallel with other requests, use carefully.
   */
  def withSharedCompiler[T](
      default: T,
      uri: String
  )(
      thunk: CompilerWrapper[Reporter, Compiler] => T
  )(implicit queryInfo: PcQueryContext): T = {
    try {
      // if (params.exists(_.text().contains("$make" + " it crash$")))
      //   throw new Exception("$make" + " it crash$")
      val compiler =
        try Right(loadCompiler())
        catch { case _: CancellationException => Left(default) }
      compiler.map(thunk).getOrElse(default)
    } catch {
      case InterruptException() =>
        default
      case ex: Throwable =>
        val exStr = {
          val baos = new ByteArrayOutputStream
          val ps = new PrintStream(baos, true, StandardCharsets.UTF_8)
          ex.printStackTrace(ps)
          new String(baos.toByteArray, StandardCharsets.UTF_8)
        }
        userLogger.accept("Caught exception")
        userLogger.accept(exStr)

        latestExceptionOpt = Some((Some(uri).filter(_.nonEmpty), ex))

        if (java.lang.Boolean.getBoolean("plasmon.enable-interactive-retry"))
          handleSharedCompilerException(ex)
            .map { message =>
              retryWithCleanCompiler(
                thunk,
                default,
                message,
                ex
              )
            }
            .getOrElse {
              handleError(ex)
              default
            }
        else
          default
    }
  }

  // TODO: make this return `String`, we should not hide meaningful exceptions
  protected def handleSharedCompilerException(t: Throwable): Option[String]

  protected def ignoreException(t: Throwable): Boolean

  private def retryWithCleanCompiler[T](
      thunk: CompilerWrapper[Reporter, Compiler] => T,
      default: T,
      message: String,
      cause: Throwable
  )(implicit queryInfo: PcQueryContext): T = {
    shutdownCurrentCompiler()
    logger.warn(
      s"compiler crashed due to $message, retrying with new compiler instance.",
      cause
    )
    try thunk(loadCompiler())
    catch {
      case InterruptException() =>
        default
      case NonFatal(e) =>
        handleError(e)
        default
    }
  }

  private def handleError(
      e: Throwable
  )(implicit queryInfo: PcQueryContext): Unit = {
    queryInfo.report("compiler-error", e, "")
    shutdownCurrentCompiler()
  }

  private def onCompilerJobQueue[T](
      name: String,
      uri: String,
      thunk: () => T,
      token: CancelToken,
      customTimeout: Option[Duration] = None
  ): CompletableFuture[T] = {
    val result = new CompletableFuture[T]()
    jobs.submit(
      name,
      uri,
      result,
      { () =>
        try {
          if (token.isCanceled()) {
            result.completeExceptionally(new CancellationException())
          } else {
            Thread.interrupted() // clear interrupt bit
            val accessId = UUID.randomUUID().toString
            beforeAccess0.foreach(_(accessId, name, uri))
            inQueuedJob.set(true)
            try result.complete(thunk())
            finally {
              inQueuedJob.set(false)
              // A job queued before a shutdown runs on a compiler of its own, stopped right after
              if (isShutdown) shutdownCurrentCompiler()
              afterAccess0.foreach(_(accessId, name, uri))
            }
          }
        } catch {
          case InterruptException() =>
            // a cancellation, not a compiler failure
            result.cancel(false)
          case NonFatal(e) =>
            logger.error(s"presentation compiler error ${e.getMessage()}", e)
            result.completeExceptionally(e)
        }
      }
    )

    // User cancelled task.
    token.onCancel.whenCompleteAsync(
      (isCancelled, _) => {
        if (isCancelled && !result.isDone()) {
          result.cancel(false)
        }
      },
      ec
    )
    // Task has timed out, cancel this request and shutdown the current compiler.
    sh.foreach { scheduler =>
      scheduler.schedule[Unit](
        { () =>
          if (!result.isDone()) {
            try {
              if (shouldResetJobQueue) jobs.reset()
              result.cancel(false)
              shutdownCurrentCompiler()
            } catch {
              case NonFatal(_) =>
              case other: Throwable =>
                if (!ignoreException(other)) throw other
            }
          }
        },
        customTimeout.map(_.getSeconds()).getOrElse(config.timeoutDelay()),
        customTimeout.map(_ => TimeUnit.SECONDS).getOrElse(config.timeoutUnit())
      )
    }

    result
  }
}
