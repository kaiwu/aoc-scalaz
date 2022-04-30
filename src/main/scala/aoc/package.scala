import aoc.util.PPtr

import scala.Conversion
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.scalanative.unsafe.{CBool, CChar, CInt, CSize, Ptr, Tag}

package object aoc {
  @tailrec
  def fold[T: Tag, A](p: Ptr[T], a: A, f: (A, Ptr[T]) => A, g: Ptr[T] => CBool): (A, Ptr[T]) =
    if (g(p)) fold(p + 1, f(a, p), f, g)
    else (a, p)

  def get_number(pptr: PPtr[CChar], p: Ptr[CInt]): Unit = {
    !p = 0
    val f = (x: CInt, p: Ptr[CChar]) => x * 10 + !p - '0'
    val g = (p: Ptr[CChar]) => !p >= '0' && !p <= '9'
    val t = fold(!pptr, !p, f, g)
    !p = t._1
    !pptr = t._2
  }
}
