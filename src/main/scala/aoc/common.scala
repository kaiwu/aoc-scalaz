package aoc

import scala.Conversion
import scala.scalanative.unsafe._
import scala.scalanative.libc._
import scala.scalanative.unsigned._
import scala.annotation.tailrec
import scalaz._
import Scalaz._

// @extern
// object wyhash {
//   def make_secret(seed: CUnsignedLongLong, secret: Ptr[CUnsignedLongLong]): Unit = extern
//   def wyhash(key: Ptr[Byte], len: CSize, seed: ULong, secret: Ptr[CUnsignedLongLong]): CUnsignedLongLong = extern
// }

type Span[T] = CStruct2[Ptr[T], CSize]
type NArray[T, N <: Nat] = CArray[T, N]
type PPtr[T] = Ptr[Ptr[T]]

object Span {
  def make[T : Tag](p: Ptr[T], s: CSize) : Ptr[Span[T]] =  {
    val span = stdlib.malloc(sizeof[Span[T]]).asInstanceOf[Ptr[Span[T]]]
    (!span)._1 = p
    (!span)._2 = s
    span
  }
  def make[T : Tag](p1: Ptr[T], p2: Ptr[T]) : Ptr[Span[T]] = make(p1, (p2 - p1).asInstanceOf[CSize])
  def make(p: CString): Ptr[Span[CChar]] = make(p, string.strlen(p))
  def apply[T : Tag](p: Ptr[T], s: CSize)(using ptr: Ptr[Span[T]]): Ptr[Span[T]] = {
    (!ptr)._1 = p
    (!ptr)._2 = s
    ptr
  }
  def apply(s: CString)(using p: Ptr[Span[CChar]]): Ptr[Span[CChar]] = {
    (!p)._1 = s
    (!p)._2 = string.strlen(s)
    p
  }
}

case class SpanOps[T: Tag](p: Ptr[Span[T]]) {
  val e: Ptr[T] = (!p)._1 + (!p)._2
  @tailrec
  final def loop(b: Ptr[T], f: Ptr[T] => CBool): Option[Ptr[T]] = {
    if (b == e) None
    else if (f(b)) Some(b)
         else loop(b + 1, f)
  }
  def length: CSize = (!p)._2
  def at(index: CSize): T = !((!p)._1 + index)
  def find(f: Ptr[T] => CBool) : Option[Ptr[T]] = loop((!p)._1, f)
  def foreach[U](f: Ptr[T] => U): Unit = ???
}

given[T: Tag]: Conversion[Ptr[Span[T]], SpanOps[T]] = new SpanOps[T](_)

object NArray {
  def apply[T: Tag, N <: Nat](args: T*)(using ptr: Ptr[CArray[T, N]], tag: Tag[CArray[T, N]]): Ptr[CArray[T, N]] = {
    var index: CSize = 0.toULong
    val total: CSize = tag.size / sizeof[T]
    string.memset(ptr.asInstanceOf[Ptr[Byte]], 0x0, tag.size)
    args.toSeq.foreach((x: T) => {
      if (index < total) {
        ptr.asInstanceOf[Ptr[T]](index) = x
        index += 1.toULong
      }
    })
    ptr
  }
}

object PPtr {
  def make[T: Tag](p: Ptr[T]) : PPtr[T] = {
    val pptr = stdlib.malloc(sizeof[Ptr[T]]).asInstanceOf[PPtr[T]]
    !pptr = p
    pptr
  }

  def apply[T: Tag](p: Ptr[T])(using pptr: PPtr[T]): PPtr[T] = {
    !pptr = p
    pptr
  }
}

object common {
  def deref[T: Tag](p: Ptr[T]): T = !p

  @tailrec
  def fold[T: Tag, A](p: Ptr[T], a: A, f : (A, Ptr[T]) => A, g: Ptr[T] => CBool) : (A, Ptr[T]) =
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