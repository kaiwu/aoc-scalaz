package aoc

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.unsafe._
import scala.scalanative.unsafe.Tag
import scala.scalanative.libc._
import scala.scalanative.unsigned._

class commonTest {
  @Test def size(): Unit = {
    assertEquals(sizeof[Span[CInt]], 16.toULong)
    assertEquals(alignmentof[Span[CInt]], 8.toULong)
    assertEquals(sizeof[Array5[CInt]], 20.toULong)
    assertEquals(sizeof[Array5[CInt]] / sizeof[CInt], 5.toULong)
    assertEquals(sizeof[Array5[Span[CInt]]], 80.toULong)
    assertEquals(sizeof[PPtr[CInt]], 8.toULong)
  }

  @Test def number(): Unit = Zone { implicit z =>
    import common.deref
    import common.get_number
    implicit def pptr[T] : PPtr[T] = alloc[Ptr[T]]()

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

  @Test def span():Unit = Zone { implicit z =>
    implicit def span[T] : Ptr[Span[T]] = alloc[Span[T]]()
    val s1 = Span(c"abc")
    val s2 = Span(c"hello")

    assertEquals(s1._2, 3.toULong)
    assertEquals(!s1._1, 'a')
    assertEquals(s2._2, 5.toULong)
    assertEquals(!s2._1, 'h')
  }
}