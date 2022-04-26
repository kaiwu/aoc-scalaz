package aoc

import org.junit.Assert.*
import org.junit.Test

import javax.print.CancelablePrintJob
import scala.scalanative.unsafe.*
import scala.scalanative.libc.*
import scala.scalanative.unsigned.*

class common {
  @Test def sizes(): Unit = {
    assertEquals(sizeof[Span[CInt]], 16.toULong)
    assertEquals(alignmentof[Span[CInt]], 8.toULong)
    assertEquals(sizeof[Array5[CInt]], 20.toULong)
    assertEquals(sizeof[Array5[CInt]] / sizeof[CInt], 5.toULong)
    assertEquals(sizeof[Array5[Span[CInt]]], 80.toULong)
    assertEquals(sizeof[PPtr[CInt]], 8.toULong)
  }
}


