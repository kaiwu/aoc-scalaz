package aoc.util
import org.junit.Assert.{assertEquals, assertFalse, assertSame, assertTrue}
import org.junit.Test
import scalaz.Order
import scalaz.Ordering.{EQ, LT, GT}

import scala.language.implicitConversions
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scala.scalanative.libc.*

class CHashMapTest {
  given cint_order_instance: Order[CInt] with {
    override def order(x: CInt, y: CInt): scalaz.Ordering = x - y match {
      case a if a == 0 => EQ
      case a if a < 0  => LT
      case a if a > 0  => GT
    }
  }

  @Test def create(): Unit = {
    val m = CHashMap[CInt, CChar]()
    m.insert(1, 'A')
    m.insert(2, 'B')
    assertEquals(m.size, 2.toULong)
  }
}
