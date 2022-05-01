package aoc.util

import scala.Conversion
import scala.language.implicitConversions
import scala.scalanative.libc._
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import aoc.util.PPtr

type DoubleLink = CStruct2[PPtr[Byte], PPtr[Byte]]
type CList[T]   = CStruct2[DoubleLink, T]

final case class DoubleLinkOps(d: Ptr[DoubleLink]) {
  def prev: Ptr[DoubleLink]            = !d.at1.asInstanceOf[PPtr[DoubleLink]]
  def next: Ptr[DoubleLink]            = !d.at2.asInstanceOf[PPtr[DoubleLink]]
  def prev_=(p: Ptr[DoubleLink]): Unit = !d.at1.asInstanceOf[PPtr[DoubleLink]] = p
  def next_=(p: Ptr[DoubleLink]): Unit = !d.at2.asInstanceOf[PPtr[DoubleLink]] = p
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
  def add_head(node: Ptr[CList[T]]): Unit = {
    if (!node.valid) node.create()
    add(node.link, h.link, h.link.next)
  }
  def add_tail(node: Ptr[CList[T]]): Unit = {
    if (!node.valid) node.create()
    add(node.link, h.link.prev, h.link)
  }
  def del(node: Ptr[CList[T]]): Unit = {
    node.link.prev.next = node.link.next
    node.link.next.prev = node.link.prev
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
