package aoc.util
import org.junit.Assert.{assertEquals, assertFalse, assertSame, assertTrue}
import org.junit.Test

import scala.language.implicitConversions
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scala.scalanative.libc.*
import aoc.util.CList.::

class CListTest {
  @Test def create(): Unit = Zone { implicit z =>
    implicit def allocator[T]: Ptr[CList[T]] = alloc[CList[T]]()
  }
}
