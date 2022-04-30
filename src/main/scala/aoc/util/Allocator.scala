package aoc
package util

import scala.scalanative.unsafe.{CSize, Ptr, Tag}
import scala.scalanative.libc.stdlib
import scala.scalanative.unsigned._

trait Allocator[T: Tag] {
  def alloc(n: CSize = 1.toULong)(using tag: Tag[T]): Ptr[T]
  def free(p: Ptr[T]): Unit
}

object Allocator {
  given [T: Tag]: Allocator[T] = new {
    override def alloc(n: CSize)(using tag: Tag[T]): Ptr[T] = stdlib.malloc(tag.size * n).asInstanceOf[Ptr[T]]
    override def free(p: Ptr[T]): Unit                      = stdlib.free(p.asInstanceOf[Ptr[Byte]])
  }
}
