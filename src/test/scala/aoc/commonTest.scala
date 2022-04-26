package aoc

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.unsafe._
import scala.scalanative.libc._
import scala.scalanative.unsigned._

class commonTest {
  @Test def sizes(): Unit = {
    assertEquals(sizeof[Span[CInt]], 16.toULong)
    assertEquals(alignmentof[Span[CInt]], 8.toULong)
    assertEquals(sizeof[Array5[CInt]], 20.toULong)
    assertEquals(sizeof[Array5[CInt]] / sizeof[CInt], 5.toULong)
    assertEquals(sizeof[Array5[Span[CInt]]], 80.toULong)
    assertEquals(sizeof[PPtr[CInt]], 8.toULong)
  }

  @Test def number(): Unit = {
    import common.dereference
    import common.get_number

    val dp = stackalloc[CInt]()
    val xp = stackalloc[Ptr[CChar]]()
    !xp = c"123"
    !dp = 0
    get_number(xp, dp)
    assertEquals(dereference(dp), 123)
    assertEquals(dereference(dereference(xp)), 0)
 }
}
