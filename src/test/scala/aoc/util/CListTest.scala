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
    val l = CList(0, 1, 2, 3, 4)
    val (max, min, total, num) = l.fold(
      (Int.MinValue, Int.MaxValue, 0, 0),
      (b, ms) => {
        (if (ms._1 > b) ms._1 else b, if (ms._2 > b) b else ms._2, ms._3 + b, ms._4 + 1)
      }
    )
    assertEquals(max, 4)
    assertEquals(min, 0)
    assertEquals(total, 10)
    assertEquals(num, 5)
  }
}
