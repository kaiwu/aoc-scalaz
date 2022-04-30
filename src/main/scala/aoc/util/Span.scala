package aoc
package util

import scalaz.*
import Scalaz.*

import scala.Conversion
import scala.language.implicitConversions
import scala.annotation.{tailrec, targetName}
import scala.scalanative.libc.{stdio, stdlib, string}
import scala.scalanative.unsafe.{CArray, CBool, CChar, CInt, CSize, CString, CStruct2, Nat, Ptr, Tag, Zone, sizeof}
import scala.scalanative.unsigned.*
import scala.math.min
import aoc.util.Allocator

type Span[T] = CStruct2[Ptr[T], CSize]

object Span {
  def make[T: Tag](p: Ptr[T], s: CSize): Span[T] = {
    val alloc         = summon[Allocator[Span[T]]]
    val span: Span[T] = alloc.alloc()
    span._1 = p
    span._2 = s
    span
  }
  def make[T: Tag](p1: Ptr[T], p2: Ptr[T]): Span[T] = make(p1, (p2 - p1).asInstanceOf[CSize])
  def make(p: CString): Span[CChar]                 = make(p, string.strlen(p))
  def apply[T: Tag](p: Ptr[T], s: CSize)(using span: Span[T]): Span[T] = {
    span._1 = p
    span._2 = s
    span
  }
  def apply[T: Tag](p1: Ptr[T], p2: Ptr[T])(using span: Span[T]): Span[T] = apply(p1, (p2 - p1).asInstanceOf[CSize])
  def apply(s: CString)(using span: Span[CChar]): Span[CChar] = {
    span._1 = s
    span._2 = string.strlen(s)
    span
  }
  def apply[T: Tag, N <: Nat](a: CArray[T, N])(using s: Span[T], tag: Tag[CArray[T, N]]): Span[T] = {
    s._1 = a.at(0)
    s._2 = tag.size / sizeof[T]
    s
  }

  given span_functor_evidence: Functor[Span] with {
    override def map[A, B](fa: Span[A])(f: A => B): Span[B] = fa.map(f)
  }
}

final case class SpanOps[T: Tag](sp: Span[T]) {
  @tailrec
  final def loop(b: Ptr[T], f: T => CBool): Option[Ptr[T]] = {
    val e: Ptr[T] = sp._1 + sp._2
    if (b == e) None
    else if (f(!b)) Some(b)
    else loop(b + 1, f)
  }
  @tailrec
  final def loop2(b1: Ptr[T], b2: Ptr[T], f: (T, T) => CBool): Option[Ptr[T]] = {
    val e: Ptr[T] = sp._1 + sp._2
    if (b1 == e) None
    else if (f(!b1, !b2)) Some(b1)
    else loop2(b1 + 1, b2 + 1, f)
  }

  def length: CSize                        = sp._2
  def isEmpty: CBool                       = sp._1 == null || length == 0.toULong
  def apply(index: CSize): T               = !(sp._1 + index)
  def apply(index: CInt): T                = !(sp._1 + index)
  def update(index: CSize, value: T): Unit = sp._1(index) = value
  def offset(index: CSize): Ptr[T]         = sp._1 + index
  def at(index: CSize): Ptr[T]             = offset(index)
  def find(f: T => CBool): Option[Ptr[T]]  = loop(sp._1, f)
  def foreach[U](f: T => U): Unit          = loop(sp._1, x => { f(x); false }).fold({})(_ => {})
  def is_same(other: Span[T]): CBool = {
    if (length == other.length) loop2(sp._1, other._1, (p1, p2) => { p1 != p2 }).isEmpty
    else false
  }
  def drop(s: CSize)(implicit span: Span[T]): Span[T] = {
    if (s >= length) {
      span._1 = null
      span._2 = 0.toULong
    } else {
      span._1 = sp._1 + s
      span._2 = length - s
    }
    span
  }
  def dropRight(s: CSize)(implicit span: Span[T]): Span[T] = {
    if (s >= length) {
      span._1 = null
      span._2 = 0.toULong
    } else {
      span._1 = sp._1
      span._2 = length - s
    }
    span
  }
  def take(s: CSize)(implicit span: Span[T]): Span[T] = {
    if (s >= length) {
      span._1 = sp._1
      span._2 = sp._2
    } else {
      span._1 = sp._1
      span._2 = s
    }
    span
  }
  def takeWhile(f: T => CBool)(implicit span: Span[T]): Span[T] = {
    span._1 = sp._1
    var index: CSize = 0.toULong
    while (index < length && f(apply(index))) {
      index += 1.toULong
    }
    span._2 = index
    span
  }
  def takeUntil(f: T => CBool)(implicit span: Span[T]): Span[T] = takeWhile(x => !f(x))
  def map[T1: Tag](f: T => T1, span: Span[T1]): Span[T1] = {
    val m     = min(length.toLong, span.length.toLong)
    var index = 0.toULong
    while (index < m.toULong) {
      span(index) = f(apply(index))
      index += 1.toULong
    }
    span
  }
  def map[T1: Tag](f: T => T1): Span[T1] = {
    val alloc = summon[Allocator[T1]]
    val ptr   = alloc.alloc(length)
    map(f, Span.make(ptr, length))
  }
}

given [T: Tag]: Conversion[Span[T], SpanOps[T]] = new SpanOps[T](_)
