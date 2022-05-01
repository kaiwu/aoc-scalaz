package aoc.util
import org.junit.Assert.{assertEquals, assertFalse, assertSame, assertTrue}
import org.junit.Test

import scala.language.implicitConversions
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scala.scalanative.libc.*

class CListTest {
  @Test def create(): Unit = Zone { implicit z =>
    val head = alloc[CList[CInt]]()
    head.create()
    assertTrue(head.valid)
    assertTrue(head.empty)
    for (i <- 1 to 10) {
      val a = alloc[CList[CInt]]()
      !a.value = i
      head.add_tail(a)
    }
    assertFalse(head.empty)
    assertEquals(head.size, 10.toULong)
  }
}
