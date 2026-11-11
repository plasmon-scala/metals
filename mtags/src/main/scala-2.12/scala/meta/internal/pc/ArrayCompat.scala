package scala.meta.internal.pc

import scala.collection.mutable.WrappedArray

object ArrayCompat {

  /** Wraps an array in a `Seq`, without copying it */
  def unsafeWrapArray[T](array: Array[T]): Seq[T] =
    WrappedArray.make(array)
}
