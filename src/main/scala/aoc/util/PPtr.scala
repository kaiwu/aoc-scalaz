package aoc
package util

import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.{Ptr, Tag, sizeof}

type PPtr[T] = Ptr[Ptr[T]]

object PPtr {
  def make[T: Tag](p: Ptr[T]) : PPtr[T] = {
    val pptr = stdlib.malloc(sizeof[Ptr[T]]).asInstanceOf[PPtr[T]]
    !pptr = p
    pptr
  }

  def apply[T: Tag](p: Ptr[T])(using pptr: PPtr[T]): PPtr[T] = {
    !pptr = p
    pptr
  }
}

