package aoc.util
import aoc.util.clist.*
import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import scala.language.implicitConversions
import scala.scalanative.unsafe.CStruct2
import scala.scalanative.unsafe.{CArray, CChar, CInt, Nat, Ptr, Tag, Zone, alloc, stackalloc}
import scala.scalanative.unsigned._
import aoc.util.given_Conversion_DoubleLink_DoubleLinkOps

type A = CList[CInt]

class CListTest {
  @Test def create(): Unit = Zone { implicit z =>
    val head = stackalloc[DoubleLink]()
    init_list_head(head)
    assertEquals(list_empty(head), 1)

    val a1 = stackalloc[A]()
    val a2 = stackalloc[A]()
    a1._2 = 1
    list_add(a1.link, head)
    a2._2 = 2
    list_add(a2.link, head)

    assertEquals(list_empty(head), 0)
  }
}
