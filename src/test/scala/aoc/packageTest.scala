package aoc

import aoc.util.{PPtr, Span}
import org.junit.Assert.assertEquals
import org.junit.Test

import scala.scalanative.unsafe._
import scala.scalanative.libc._
import scala.scalanative.unsigned._

class packageTest {
  @Test def size(): Unit = {
    assertEquals(sizeof[Span[CInt]], 16.toULong)
    assertEquals(alignmentof[Span[CInt]], 8.toULong)
    assertEquals(sizeof[CArray[CInt, Nat._5]], 20.toULong)
    assertEquals(sizeof[CArray[CInt, Nat._5]] / sizeof[CInt], 5.toULong)
    assertEquals(sizeof[CArray[Span[CInt], Nat._5]], 80.toULong)
    assertEquals(sizeof[PPtr[CInt]], 8.toULong)
  }

  @Test def number(): Unit = Zone { implicit z =>
    implicit def allocator[T]: PPtr[T] = alloc[Ptr[T]]()

    val dp = stackalloc[CInt]()
    val xp1 = PPtr(c"123")
    get_number(xp1, dp)
    assertEquals(deref(dp), 123)
    assertEquals(deref(deref(xp1)), 0)

    val xp2 = PPtr(c"456")
    get_number(xp2, dp)
    assertEquals(deref(dp), 456)
    assertEquals(deref(deref(xp2)), 0)
  }
}