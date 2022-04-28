package aoc.util

import scala.language.implicitConversions
import org.junit.Assert._
import org.junit.Test

import scala.scalanative.unsafe._
import scala.scalanative.libc._
import scala.scalanative.unsigned._

class SpanTest {
  @Test def span(): Unit = Zone { implicit z =>
    implicit def allocator[T]: Ptr[Span[T]] = alloc[Span[T]]()
    val s1 = Span(c"abc")
    val s2 = Span(c"hello")

    assertEquals(s1.at(0.toULong), 'a')
    assertEquals(s1.length, 3.toULong)
    assertEquals(s2.at(0.toULong), 'h')
    assertEquals(s2.length, 5.toULong)
  }
}