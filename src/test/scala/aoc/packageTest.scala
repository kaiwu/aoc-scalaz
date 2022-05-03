package aoc

import aoc.util.{PPtr, Span, SpanOps}
import org.junit.Assert.assertEquals
import org.junit.Test

import scala.Conversion
import scala.language.implicitConversions
import scala.scalanative.unsafe.*
import scala.scalanative.libc.*
import scala.scalanative.unsigned.*

class packageTest {
  @Test def size(): Unit = {
    assertEquals(sizeof[Span[CInt]], 16.toULong)
    assertEquals(alignmentof[Span[CInt]], 8.toULong)
    assertEquals(sizeof[CArray[CInt, Nat._5]], 20.toULong)
    assertEquals(sizeof[CArray[CInt, Nat._5]] / sizeof[CInt], 5.toULong)
    assertEquals(sizeof[CArray[Span[CInt], Nat._5]], 80.toULong)
    assertEquals(sizeof[PPtr[CInt]], 8.toULong)
  }

  @Test def struct(): Unit = {
    type S = CStruct2[CInt, CInt]

    def method(s: S): S = { s._2 = s._2 + 1; s }
    val x               = stackalloc[S]()
    x._1 = 10
    x._2 = 20
    val r = method(x)

    assertEquals(r._1, 10)
    assertEquals(r._2, 21)
  }

  @Test def file(): Unit = {
    val f  = load_file(c"C:\\Tmp\\CMakeCache.txt")
    val sp = SpanOps(f)
    stdio.printf(c"%zu\n", sp.length)
  }

  @Test def number(): Unit = Zone { implicit z =>
    implicit def allocator[T]: PPtr[T] = alloc[Ptr[T]]()

    val dp  = stackalloc[CInt]()
    val xp1 = PPtr(c"123")
    get_number(xp1, dp)
    assertEquals(!dp, 123)
    assertEquals(!(!xp1), 0)

    val xp2 = PPtr(c"456")
    get_number(xp2, dp)
    assertEquals(!dp, 456)
    assertEquals(!(!xp2), 0)
  }
}
