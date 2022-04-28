package aoc

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.unsafe._
import scala.scalanative.libc._
import scala.scalanative.unsigned._

class commonTest {
  @Test def size(): Unit = {
    assertEquals(sizeof[Span[CInt]], 16.toULong)
    assertEquals(alignmentof[Span[CInt]], 8.toULong)
    assertEquals(sizeof[CArray[CInt, Nat._5]], 20.toULong)
    assertEquals(sizeof[CArray[CInt, Nat._5]] / sizeof[CInt], 5.toULong)
    assertEquals(sizeof[CArray[Span[CInt], Nat._5]], 80.toULong)
    assertEquals(sizeof[PPtr[CInt]], 8.toULong)
  }

  @Test def number(): Unit = Zone { implicit z =>
    import common.deref
    import common.get_number
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

  @Test def span(): Unit = Zone { implicit z =>
    implicit def allocator[T]: Ptr[Span[T]] = alloc[Span[T]]()
    val s1 = Span(c"abc")
    val s2 = Span(c"hello")

    assertEquals(s1._2, 3.toULong)
    assertEquals(!s1._1, 'a')
    assertEquals(s2._2, 5.toULong)
    assertEquals(!s2._1, 'h')
  }

  @Test def array(): Unit = Zone { implicit z =>
    implicit def allocator[T: Tag, N <: Nat]: Ptr[CArray[T, N]] =
      alloc[CArray[T, N]]()
    val a1 = NArray[CInt, Nat.Digit2[Nat._1, Nat._0]](1, 2, 3, 4, 5)
    assertEquals(a1.length, 10)
    for (i <- 0 until 5) assertEquals(i + 1, !a1.at(i))
    for (i <- 5 until 10) assertEquals(0, !a1.at(i))

    val a2 = NArray[CChar, Nat._4]('a', 'b', 'c', 'd', 'e', 'f')
    assertEquals(a2.length, 4)
    for (i <- 0 until 4) assertEquals('a' + i, !a2.at(i))
  }
}
