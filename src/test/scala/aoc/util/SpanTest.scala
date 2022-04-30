package aoc.util

import scala.language.implicitConversions
import org.junit.Assert._
import org.junit.Test

import scala.scalanative.unsafe._
import scala.scalanative.libc._
import scala.scalanative.unsigned._

class SpanTest {
  @Test def span(): Unit = Zone { implicit z =>
    implicit def allocator[T]: Span[T] = alloc[Span[T]]()
    val s1                             = Span(c"abc")
    val s2                             = Span(c"hello")
    val s3                             = Span(c"abchello").drop(3.toULong)

    assertEquals(s1(0), 'a')
    assertEquals(s1.length, 3.toULong)
    assertEquals(s2(0), 'h')
    assertEquals(s2.length, 5.toULong)
    assertFalse(s1.is_same(s2))
    assertFalse(s2 == s3)
    assertTrue(s3.is_same(s2))
    assertTrue(s1.find(_ == 'A').isEmpty)
    assertFalse(s2.find(_ == 'l').isEmpty)

    var x = 0
    s1.foreach(_ => x += 1)
    assertEquals(x, 3)
    s2.foreach(_ => x += 10)
    assertEquals(x, 53)

    assertTrue(s2.take(3.toULong).is_same(Span(c"hel")))
    assertTrue(s2.takeUntil(_ == 'o').is_same(Span(c"hell")))
    assertTrue(s3.takeWhile(_ != ' ').is_same(Span(c"hello")))
    assertTrue(s2.drop(5.toULong).isEmpty)
    assertTrue(s2.take(0.toULong).isEmpty)
    val f: CChar => CChar = x => (x - 32).asInstanceOf[CChar]
    assertTrue(s1.map(f).is_same(Span(c"ABC")))

    val a1 = alloc[CInt](3)
    for (i <- 0 until 3) !(a1 + i) = i
    val s4               = Span(a1, 3.toULong)
    val g: CInt => CChar = x => (x + 'A').asInstanceOf[CChar]
    assertTrue(s4.map(g).is_same(Span(c"ABC")))
  }
}
