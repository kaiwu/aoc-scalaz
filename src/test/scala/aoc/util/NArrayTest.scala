package aoc.util

import org.junit.Assert.assertEquals
import org.junit.Test
import scala.scalanative.unsafe.{CArray, CChar, CInt, Nat, Ptr, Tag, Zone, alloc}

class NArrayTest {
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
