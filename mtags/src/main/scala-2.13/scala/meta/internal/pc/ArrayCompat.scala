package scala.meta.internal.pc

import scala.collection.immutable.ArraySeq

object ArrayCompat {

  /** Wraps an array in a `Seq`, without copying it */
  def unsafeWrapArray[T](array: Array[T]): Seq[T] =
    ArraySeq.unsafeWrapArray(array)
}
