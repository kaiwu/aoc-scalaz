import aoc.util.PPtr
import aoc.util.Span
import aoc.util.Allocator

import scala.Conversion
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.scalanative.libc._
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._

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

  def load_file(path: CString): Span[Byte] = {
    val file = stdio.fopen(path, c"r")
    @tailrec
    def file_size(file: Ptr[stdio.FILE], x: CSize): CSize = {
      val buf = stackalloc[Byte](1024)
      val s   = stdio.fread(buf, sizeof[Byte], 1024.toULong, file)
      if (s < 1024.toULong) x + s
      else file_size(file, x + s)
    }

    if (file == null) Span.make(null: Ptr[Byte], 0.toULong)
    else {
      var size = file_size(file, 0.toULong)
      val ptr  = summon[Allocator[Byte]].alloc(size)
      stdio.fseek(file, 0.toLong, stdio.SEEK_SET)
      stdio.fread(ptr, sizeof[Byte], size, file)
      Span.make(ptr, size)
    }
  }
}
