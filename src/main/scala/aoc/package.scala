import aoc.util.PPtr
import aoc.util.Span
import aoc.util.Allocator

import scala.Conversion
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.scalanative.libc._
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scala.scalanative.posix.sys.stat._
import scala.scalanative.posix.sys.stat

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

  def load_file(path: CString): Span[CChar] = {
    val file      = stdio.fopen(path, c"r")
    val file_stat = stackalloc[stat]()
    val s         = stat.stat(path, file_stat)
    if (file == null || s != 0) Span.make(null: Ptr[CChar], 0.toULong)
    else {
      val size: CSize = file_stat._6.asInstanceOf[CSize]
      val alloc       = summon[Allocator[CChar]]
      val buf         = alloc.alloc(size)
      val rs          = stdio.fread(buf, sizeof[Byte], size, file)
      Span.make(buf, size)
    }
  }
}
