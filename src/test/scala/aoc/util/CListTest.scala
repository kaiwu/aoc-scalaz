package aoc.util
import org.junit.Assert.{assertEquals, assertFalse, assertSame, assertTrue}
import org.junit.Test

import scala.language.implicitConversions
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scala.scalanative.libc.*
import aoc.util.CList.head

class CListTest {
  @Test def create(): Unit = Zone { implicit z =>
    // implicit def allocator[T]: Ptr[CList[T]] = alloc[CList[T]]()
    val l1 = CList(1, 2, 3)
    val l2 = CList(4, 5, 6)
    val l3 = CList(7, 8, 9)
    val l  = l1 :: l2 :: l3
    assertEquals(l.size, 9.toULong)
    // l.foreach(stdio.printf(c"%d", _))
  }
}
