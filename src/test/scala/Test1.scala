import org.junit.Assert._
import org.junit.Test

import aoc.common

class Test1 {
 @Test def plus1(): Unit = {
   assertTrue(common.plus(1 , 2) == 3)
   assertTrue(common.plus(10 , 2) == 12)
 }
}
