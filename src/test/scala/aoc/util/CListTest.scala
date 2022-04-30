package aoc.util
import aoc.util.clist.*
import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import scala.language.implicitConversions
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scala.scalanative.libc._

class CListTest {
  @Test def create(): Unit = Zone { implicit z =>
    val head = alloc[CList[CInt]]()
    head.create()
    for (i <- 1 to 10) {
      val a = alloc[CList[CInt]]()
      !a.value = i
      head.add_tail(a)
    }
    head.foreach(stdio.printf(c"%d -> ", _))
  }
}
