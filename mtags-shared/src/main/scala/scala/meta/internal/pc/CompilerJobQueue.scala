package scala.meta.internal.pc

import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture
import java.util.concurrent.PriorityBlockingQueue
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import java.{util => ju}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ThreadFactory

import scala.jdk.CollectionConverters._

/**
 * A thread pool executor to execute jobs on a single thread in a last-in-first-out order.
 *
 * The last-in-first-out order is important because it's common for Metals
 * users to send multiple completion/hover/signatureHelp requests in rapid
 * succession. In these situations, we care most about responding to the latest
 * request even if it comes at the expense of ignoring older requests.
 *
 * To restrict unsafe multi-threaded access to the presentation compiler we
 * schedule jobs to run on a single thread. We use this executor instead of the
 * presentation compiler thread (see MetalsGlobalThread) for the following reasons:
 * - we limit the usage of sleep/notify/wait/synchronized primitives.
 * - some blocking compiler APIs like `ask[T](op: () => T)` don't seem to work as advertised.
 * - it's preferable to work on top of CompletableFuture[T] instead of the custom Response[T] from the compiler,
 *   which is required to execute tasks on the presentation compiler thread:
 *   - CompletableFuture[T] can be passed via Java-only reflection APIs in mtags-interfaces.
 *   - CompletableFuture[T] can be returned to lsp4j, for non-blocking JSON-RPC request handling.
 *   - CompletableFuture[T] can be converted to Scala Futures for easier composition.
 */
class CompilerJobQueue(newExecutor: () => ThreadPoolExecutor) {
  import CompilerJobQueue.State

  private val state = new AtomicReference[State](State.Empty)

  def submit(name: String, uri: String, fn: () => Unit): Unit = {
    submit(name, uri, new CompletableFuture[Unit](), fn)
  }
  def submit(
      name: String,
      uri: String,
      result: CompletableFuture[_],
      fn: () => Unit
  ): Unit = {
    onExecutor(
      _.execute(new CompilerJobQueue.Job(name, uri, result, fn)),
      () => result.completeExceptionally(new CancellationException())
    )
  }

  private def onExecutor[A](
      f: ThreadPoolExecutor => A,
      fallback: () => A
  ): A = {
    state.get() match {
      case State.Empty =>
        if (state.compareAndSet(State.Empty, State.Initializing)) {
          val value = newExecutor()
          state.set(State.Initialized(value))
          f(value)
        } else {
          delay()
          onExecutor(f, fallback)
        }
      case State.Initialized(v) => f(v)
      case State.Initializing =>
        delay()
        onExecutor(f, fallback)
      case State.Stopped => fallback()
    }
  }

  def shutdown(): Unit = {
    state.get() match {
      case State.Empty =>
        if (!state.compareAndSet(State.Empty, State.Stopped)) {
          delay()
          shutdown()
        }
      case curr @ State.Initialized(v) =>
        if (state.compareAndSet(curr, State.Stopped)) {
          v.shutdown()
        } else {
          delay()
          shutdown()
        }
      case State.Initializing =>
        delay()
        shutdown()
      case State.Stopped =>
    }
  }

  def reset(): Unit = {
    state.get() match {
      case curr @ State.Initialized(v) =>
        if (state.compareAndSet(curr, State.Empty)) {
          v.shutdown()
        } else {
          delay()
          reset()
        }
      case _ =>
    }
  }

  def runningJob(): Option[CompilerJobQueue.Job] =
    state.get() match {
      case State.Initialized(v) =>
        v.asInstanceOf[CompilerJobQueue.HasRunningJob].runningJobOpt
      case _ =>
        None
    }

  def jobQueue(): List[CompilerJobQueue.Job] =
    state.get() match {
      case State.Initialized(v) =>
        v.getQueue()
          .asScala
          .toList
          .map {
            case job: CompilerJobQueue.Job => job
            case _ => sys.error("Cannot happen")
          }
      case _ =>
        Nil
    }

  private def delay(): Unit = Thread.sleep(50)

  override def toString(): String = s"CompilerJobQueue(${state.get})"

  // The implementation of `Executors.newSingleThreadExecutor()` uses finalize.
  override def finalize(): Unit = {
    shutdown()
  }
}

object CompilerJobQueue {

  sealed trait State
  object State {
    case object Empty extends State
    final case class Initialized(v: ThreadPoolExecutor) extends State
    case object Initializing extends State
    case object Stopped extends State
  }

  trait HasRunningJob {
    def runningJobOpt: Option[Job]
  }

  private val instanceNumber = new AtomicInteger(1)
  def apply(): CompilerJobQueue = {
    new CompilerJobQueue(() => {
      val singleThreadExecutor = new ThreadPoolExecutor(
        /* corePoolSize */ 1,
        /* maximumPoolSize */ 1,
        /* keepAliveTime */ 0,
        /* unit */ TimeUnit.MILLISECONDS,
        /* workQueue */ new LastInFirstOutBlockingQueue,
        /* factory */ new ThreadFactory {
          val threadNumber = new AtomicInteger(1)
          def newThread(r: Runnable) = {
            val t = new Thread(
              r,
              s"compiler-job-queue-${instanceNumber.getAndIncrement()}-" +
                s"thread-${threadNumber.getAndIncrement()}"
            )
            t.setDaemon(true)
            t.setPriority(Thread.NORM_PRIORITY)
            t
          }
        }
      ) with HasRunningJob {
        def runningJobOpt = running
        private var running = Option.empty[Job]
        override protected def beforeExecute(t: Thread, r: Runnable): Unit =
          r match {
            case job: Job =>
              running = Some(job)
            case _ =>
          }
        override protected def afterExecute(r: Runnable, t: Throwable): Unit =
          if (running.contains(r))
            running = None
      }
      singleThreadExecutor.setRejectedExecutionHandler {
        case (j: Job, _) =>
          j.reject()
        case _ =>
      }
      singleThreadExecutor
    })
  }

  /**
   * Runnable with a timestamp and attached completable future.
   */
  class Job(
      val name: String,
      val uri: String,
      val result: CompletableFuture[_],
      _run: () => Unit
  ) extends Runnable {
    def reject(): Unit = {
      result.completeExceptionally(new CancellationException("rejected"))
    }
    val start: Long = System.nanoTime()
    def run(): Unit = {
      if (!result.isDone()) {
        _run()
      }
    }
  }

  /**
   * Priority queue that runs the most recently submitted task first.
   */
  private class LastInFirstOutBlockingQueue
      extends PriorityBlockingQueue[Runnable](
        10,
        new ju.Comparator[Runnable] {
          def compare(o1: Runnable, o2: Runnable): Int = {
            // Downcast is safe because we only submit `Job` runnables into this
            // threadpool via `CompilerJobQueue.submit`. We can't make the queue
            // `PriorityBlockingQueue[Job]` because `new ThreadPoolExecutor` requires
            // a `BlockingQueue[Runnable]` and Java queues are invariant.
            -java.lang.Long.compare(
              o1.asInstanceOf[Job].start,
              o2.asInstanceOf[Job].start
            )
          }
        }
      )
}
