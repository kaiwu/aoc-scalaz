package aoc.util
import aoc.util.CList
import org.junit.Assert.assertEquals
import org.junit.Test

import scala.scalanative.unsafe.{CArray, CChar, CInt, Nat, Ptr, Tag, Zone, alloc, stackalloc}

class CListTest {
  @Test def create(): Unit = Zone { implicit z =>
    val p = stackalloc[DoubleLink]()
    CList.init_list_head(p)
  }
}
