package aoc
package util

import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.{Ptr, Tag, sizeof}
import aoc.util.Allocator

type PPtr[T] = Ptr[Ptr[T]]

object PPtr {
  def make[T: Tag](p: Ptr[T]): PPtr[T] = {
    val alloc = summon[Allocator[Ptr[T]]]
    val pptr  = alloc.alloc()
    !pptr = p
    pptr
  }

  def apply[T: Tag](p: Ptr[T])(using pptr: PPtr[T]): PPtr[T] = {
    !pptr = p
    pptr
  }
}
