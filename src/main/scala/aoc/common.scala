package aoc

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
type Array5[T] = CArray[T, Nat._5]
type PPtr[T] = Ptr[Ptr[T]]

object Span {
  def make[T : Tag](p: Ptr[T], s: CSize) : Ptr[Span[T]] =  {
    val span = stdlib.malloc(sizeof[Span[T]]).asInstanceOf[Ptr[Span[T]]]
    span._1 = p
    span._2 = s
    span
  }
  def make[T : Tag](p1: Ptr[T], p2: Ptr[T]) : Ptr[Span[T]] = make(p1, (p2 - p1).asInstanceOf[CSize])
  def make(p: CString): Ptr[Span[CChar]] = make(p, string.strlen(p))
  def apply[T](p: Ptr[T], s: CSize)(using ptr: Ptr[Span[T]]): Ptr[Span[T]] = {
      ptr._1 = p
      ptr._2 = s
      ptr
  }
  def apply(s: CString)(using p: Ptr[Span[CChar]]): Ptr[Span[CChar]] = {
    p._1 = s
    p._2 = string.strlen(s)
    p
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