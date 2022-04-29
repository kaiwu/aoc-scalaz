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
  def make[T: Tag](p: Ptr[T], s: CSize): Ptr[Span[T]] =  {
    val alloc = summon[Allocator[Span[T]]]
    val span = alloc.alloc()
    (!span)._1 = p
    (!span)._2 = s
    span
  }
  def make[T: Tag](p1: Ptr[T], p2: Ptr[T]): Ptr[Span[T]] = make(p1, (p2 - p1).asInstanceOf[CSize])
  def make(p: CString): Ptr[Span[CChar]] = make(p, string.strlen(p))
  def apply[T: Tag](p: Ptr[T], s: CSize)(using ptr: Ptr[Span[T]]): Ptr[Span[T]] = {
    (!ptr)._1 = p
    (!ptr)._2 = s
    ptr
  }
  def apply[T: Tag](p1: Ptr[T], p2: Ptr[T])(using ptr: Ptr[Span[T]]): Ptr[Span[T]] = apply(p1, (p2 - p1).asInstanceOf[CSize])
  def apply(s: CString)(using p: Ptr[Span[CChar]]): Ptr[Span[CChar]] = {
    (!p)._1 = s
    (!p)._2 = string.strlen(s)
    p
  }
  def apply[T: Tag, N <: Nat](a: CArray[T, N])(using p: Ptr[Span[T]], tag: Tag[CArray[T, N]]): Ptr[Span[T]] = {
    (!p)._1 = a.at(0)
    (!p)._2 = tag.size / sizeof[T]
    p
  }
}

case class SpanOps[T: Tag](p: Ptr[Span[T]]) {
  @tailrec
  final def loop(b: Ptr[T], f: Ptr[T] => CBool): Option[Ptr[T]] = {
    val e: Ptr[T] = (!p)._1 + (!p)._2
    if (b == e) None
    else if (f(b)) Some(b)
    else loop(b + 1, f)
  }
  @tailrec
  final def loop2(b1: Ptr[T], b2:Ptr[T], f: (Ptr[T], Ptr[T]) => CBool): Option[Ptr[T]] = {
    val e: Ptr[T] = (!p)._1 + (!p)._2
    if (b1 == e) None
    else if (f(b1, b2)) Some(b1)
    else loop2(b1 + 1, b2 + 1, f)
  }

  def length: CSize = (!p)._2
  def isEmpty: CBool = (!p)._1 == null || length == 0.toULong
  def at(index: CSize): T = !((!p)._1 + index)
  def offset(index: CSize): Ptr[T] = (!p)._1 + index
  def find(f: Ptr[T] => CBool): Option[Ptr[T]] = loop((!p)._1, f)
  def foreach[U](f: Ptr[T] => U): Unit = loop((!p)._1, x => { f(x) ; false }).fold({})(_ => {})
  def is_same(other: Ptr[Span[T]]): CBool = {
    if (length == other.length) loop2((!p)._1 ,(!other)._1, (p1, p2) => {!p1 != !p2}).isEmpty
    else false
  }
  def drop(s: CSize)(implicit ptr: Ptr[Span[T]]): Ptr[Span[T]] = {
    if (s >= length) {
      (!ptr)._1 = null
      (!ptr)._2 = 0.toULong
    }
    else {
      (!ptr)._1 = (!p)._1 + s
      (!ptr)._2 = length - s
    }
    ptr
  }
  def dropRight(s: CSize)(implicit ptr: Ptr[Span[T]]): Ptr[Span[T]] = {
    if (s >= length) {
      (!ptr)._1 = null
      (!ptr)._2 = 0.toULong
    }
    else {
      (!ptr)._1 = (!p)._1
      (!ptr)._2 = length - s
    }
    ptr
  }
  def take(s: CSize)(implicit ptr: Ptr[Span[T]]): Ptr[Span[T]] =  {
    if (s >= length) {
      !ptr = !p
    }
    else {
      (!ptr)._1 = (!p)._1
      (!ptr)._2 = s
    }
    ptr
  }
  def takeWhile(f: Ptr[T] => CBool)(implicit ptr: Ptr[Span[T]]): Ptr[Span[T]] =  {
    (!ptr)._1 = (!p)._1
    var index: CSize = 0.toULong
    while (index < length && f(offset(index))) {
      index += 1.toULong
    }
    (!ptr)._2 = index
    ptr
  }
  def takeUntil(f: Ptr[T] => CBool)(implicit ptr: Ptr[Span[T]]): Ptr[Span[T]] = takeWhile(x => !f(x))
  def map[T1: Tag](f: Ptr[T] => T1, ptr: Ptr[Span[T1]]): Ptr[Span[T1]] = {
    val m = min(length.toLong, ptr.length.toLong)
    var index = 0.toULong
    while (index < m.toULong) {
      !ptr.offset(index) = f(offset(index))
      index += 1.toULong
    }
    ptr
  }
  def map[T1: Tag](f: Ptr[T] => T1): Ptr[Span[T1]] = {
    val alloc = summon[Allocator[T1]]
    val ptr = alloc.alloc(length)
    map(f, Span.make(ptr, length))
  }
}

given[T: Tag]: Conversion[Ptr[Span[T]], SpanOps[T]] = new SpanOps[T](_)