package aoc
package util

import scala.Conversion
import scala.scalanative.unsafe._
import scala.scalanative.libc._
import scala.scalanative.unsigned._
import scala.annotation.tailrec
import scalaz._
import Scalaz._

type Span[T] = CStruct2[Ptr[T], CSize]

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
  def foreach[U](f: Ptr[T] => U): Unit = loop((!p)._1, (x: Ptr[T]) => {
    f(x)
    false
  })
}

given[T: Tag]: Conversion[Ptr[Span[T]], SpanOps[T]] = new SpanOps[T](_)
