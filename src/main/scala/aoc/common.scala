package aoc

import scala.scalanative.unsafe._
import scala.scalanative.libc._
import scala.scalanative.unsigned._
import scala.annotation.tailrec
import scalaz._
import Scalaz._

@extern
object wyhash {
  def make_secret(seed: CUnsignedLongLong, secret: Ptr[CUnsignedLongLong]): Unit = extern
  def wyhash(key: Ptr[Byte], len: CSize, seed: ULong, secret: Ptr[CUnsignedLongLong]): CUnsignedLongLong = extern
}

type Span[T] = CStruct2[Ptr[T], CSize]
type Array5[T] = CArray[T, Nat._5]
type PPtr[T] = Ptr[Ptr[T]]

object common {
  def dereference[T: Tag](p: Ptr[T]): T = !p

  @tailrec
  def fold[T: Tag, A](p: Ptr[T], a: A, f : (A, Ptr[T]) => A, g: Ptr[T] => CBool) : (A, Ptr[T]) = {
    if (g(p)) fold(p + 1, f(a, p), f, g)
    else (a, p)
  }

  def get_number(pptr: PPtr[CChar], p: Ptr[CInt]): Unit = {
    val f = (x: CInt, c: Ptr[CChar]) => x * 10 + !c - '0'
    val g = (p: Ptr[CChar]) => !p >= '0' && !p <= '9'
    val t = fold(!pptr, !p, f, g)
    !p = t._1
    !pptr = t._2
  }
}