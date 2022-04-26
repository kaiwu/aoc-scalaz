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
    val dp = stackalloc[CInt]()
    val xp = stackalloc[Ptr[CChar]]()
    !xp = c"123"
    common.get_number(xp, dp)
    assertEquals(!dp, 123)
    assertEquals(!(!xp), 0)

    !xp = c"456"
    common.get_number(xp, dp)
    assertEquals(!dp, 456)
    assertEquals(!(!xp), 0)
  }
}
