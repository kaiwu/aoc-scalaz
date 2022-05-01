package aoc.util

import scala.Conversion
import scala.language.implicitConversions
import scala.scalanative.libc._
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scala.annotation.targetName
import aoc.util.PPtr
import aoc.util.Allocator

type DoubleLink = CStruct2[PPtr[Byte], PPtr[Byte]]
type CList[T]   = CStruct2[DoubleLink, T]

object CList {
  implicit def nil(implicit p: Ptr[DoubleLink]): Ptr[DoubleLink] = {
    p.next = p
    p.prev = p
    p
  }
  def make[T: Tag](e: T): Ptr[CList[T]] = {
    val alloc = summon[Allocator[CList[T]]]
    val p = alloc.alloc()
    !p.value = e
    p.create()
    p
  }
  def apply[T: Tag](e: T)(implicit p: Ptr[CList[T]]): Ptr[CList[T]] = {
    !p.value = e
    p.create()
    p
  }
  def apply[T: Tag](ls: T*): Ptr[CList[T]] = ???
  @targetName("::")
  def ::[T: Tag](l1: Ptr[CList[T]], l2: Ptr[CList[T]]): Ptr[CList[T]] = {
    l1.add_tail(l2)
  }
}

final case class DoubleLinkOps(d: Ptr[DoubleLink]) {
  def prev: Ptr[DoubleLink]            = !d.at1.asInstanceOf[PPtr[DoubleLink]]
  def next: Ptr[DoubleLink]            = !d.at2.asInstanceOf[PPtr[DoubleLink]]
  def prev_=(p: Ptr[DoubleLink]): Unit = !d.at1.asInstanceOf[PPtr[DoubleLink]] = p
  def next_=(p: Ptr[DoubleLink]): Unit = !d.at2.asInstanceOf[PPtr[DoubleLink]] = p

  // d o d->next
  def con_head(o: Ptr[DoubleLink]) : Ptr[DoubleLink] = {
    d.next.prev = o
    o.next = d.next
    d.next = o
    o.prev = d
    d
  }
  // d->prev o d
  def con_tail(o: Ptr[DoubleLink]) : Ptr[DoubleLink] = {
    d.prev.next = o
    o.prev = d.prev
    o.next = d
    d.prev = o
    d
  }
}
given Conversion[Ptr[DoubleLink], DoubleLinkOps]       = DoubleLinkOps(_)
given [T: Tag]: Conversion[Ptr[CList[T]], CListOps[T]] = new CListOps[T](_)

final case class CListOps[T: Tag](h: Ptr[CList[T]]) {
  def link: Ptr[DoubleLink] = h.at1
  def value: Ptr[T]         = h.at2
  def valid: CBool          = link.next != null && link.prev != null
  def empty: CBool          = valid && link.next == link
  def create(): Unit        = { h.link.prev = h.link; h.link.next = h.link }
  def add(node: Ptr[DoubleLink], prev: Ptr[DoubleLink], next: Ptr[DoubleLink]): Unit = {
    prev.next = node
    node.prev = prev
    node.next = next
    next.prev = node
  }
  def add_head(node: Ptr[CList[T]]): Ptr[CList[T]] = {
    if (!node.valid) node.create()
    add(node.link, h.link, h.link.next)
    h
  }
  def add_tail(node: Ptr[CList[T]]): Ptr[CList[T]] = {
    if (!node.valid) node.create()
    add(node.link, h.link.prev, h.link)
    h
  }
  def del(node: Ptr[CList[T]]): Ptr[CList[T]] = {
    node.link.prev.next = node.link.next
    node.link.next.prev = node.link.prev
    h
  }

  def foreach[U](f: T => U): Unit = {
    var n = h.link.next
    while (n != h.link) {
      f(!n.asInstanceOf[Ptr[CList[T]]].value)
      n = n.next
    }
  }
  def foreach_back[U](f: T => U): Unit = {
    var p = h.link.prev
    while (p != h.link) {
      f(!p.asInstanceOf[Ptr[CList[T]]].value)
      p = p.prev
    }
  }
  def size: CSize = {
    var s: CSize = 0.toULong
    foreach(_ => s += 1.toULong)
    s
  }
}
