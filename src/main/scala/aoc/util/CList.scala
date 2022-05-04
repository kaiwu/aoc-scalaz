package aoc.util

import scalaz._
import Scalaz._

import scala.Conversion
import scala.language.implicitConversions
import scala.scalanative.libc.*
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scala.annotation.{showAsInfix, tailrec, targetName}
import aoc.util.PPtr
import aoc.util.Allocator

type DoubleLink = CStruct2[PPtr[Byte], PPtr[Byte]]
type CList[T]   = CStruct2[DoubleLink, T]

object CList {
  implicit def head[T: Tag]: CList[T] = {
    val alloc = summon[Allocator[CList[T]]]
    val n     = alloc.alloc()
    n.asInstanceOf[Ptr[DoubleLink]].reset()
    !n
  }
  def make[T: Tag](e: T)(implicit n: CList[T]): CList[T] = {
    val alloc = summon[Allocator[CList[T]]]
    val p     = alloc.alloc()
    p.asInstanceOf[Ptr[DoubleLink]].reset()
    !(!p).value = e
    n.link.con_tail((!p).link)
    n
  }
  def apply[T: Tag](ls: T*): CList[T] = {
    val n = head[T]
    ls.toSeq.foreach(x => make(x)(summon[Tag[T]], n))
    n
  }
  def make[T: Tag](l1: CList[T], l2: CList[T]): CList[T] = (l1, l2) match {
    case (x, _) if !x.valid || x.empty => l2
    case (_, x) if !x.valid || x.empty => l1
    case (x, y) =>
      val h = head[T]
      x.foreach(t => make(t)(summon[Tag[T]], h))
      y.foreach(t => make(t)(summon[Tag[T]], h))
      h
  }
  given clist_functor_instance: Functor[CList] with {
    override def map[A, B](fa: CList[A])(f: A => B): CList[B] = fa.map(f)
  }
  given [T: Tag]: Monoid[CList[T]] with {
    override def zero: CList[T]                                  = head[T]
    override def append(f1: CList[T], f2: => CList[T]): CList[T] = CList.make(f1, f2)
  }
}

final case class DoubleLinkOps(d: Ptr[DoubleLink]) {
  def prev: Ptr[DoubleLink]            = !d.at1.asInstanceOf[PPtr[DoubleLink]]
  def next: Ptr[DoubleLink]            = !d.at2.asInstanceOf[PPtr[DoubleLink]]
  def prev_=(p: Ptr[DoubleLink]): Unit = !d.at1.asInstanceOf[PPtr[DoubleLink]] = p
  def next_=(p: Ptr[DoubleLink]): Unit = !d.at2.asInstanceOf[PPtr[DoubleLink]] = p

  def reset(): Unit = { prev = d; next = d }

  // d o d->next
  def con_head(o: Ptr[DoubleLink]): Ptr[DoubleLink] = {
    d.next.prev = o
    o.next = d.next
    d.next = o
    o.prev = d
    d
  }
  // d h t d->next
  def con_head(h: Ptr[DoubleLink], t: Ptr[DoubleLink]): Ptr[DoubleLink] = {
    d.next.prev = t
    t.next = d.next
    d.next = h
    h.prev = d
    d
  }
  // d->prev o d
  def con_tail(o: Ptr[DoubleLink]): Ptr[DoubleLink] = {
    d.prev.next = o
    o.prev = d.prev
    o.next = d
    d.prev = o
    d
  }
  // d->prev h t d
  def con_tail(h: Ptr[DoubleLink], t: Ptr[DoubleLink]): Ptr[DoubleLink] = {
    d.prev.next = h
    h.prev = d.prev
    t.next = d
    d.prev = t
    d
  }
}
given Conversion[Ptr[DoubleLink], DoubleLinkOps]  = DoubleLinkOps(_)
given [T: Tag]: Conversion[CList[T], CListOps[T]] = new CListOps[T](_)

final case class CListOps[T: Tag](h: CList[T]) {
  def link: Ptr[DoubleLink] = h.at1
  def value: Ptr[T]         = h.at2
  def valid: CBool          = link.next != null && link.prev != null
  def empty: CBool          = valid && link.next == link
  def reset(): Unit         = h.link.reset()
  def del(node: CList[T]): CList[T] = {
    node.link.prev.next = node.link.next
    node.link.next.prev = node.link.prev
    h
  }

  @tailrec
  def loop[T1](p: CList[T], x: T1, f: (T, T1) => T1): T1 = {
    val n = p.link.next.asInstanceOf[Ptr[CList[T]]]
    if ((!n).link.next == h.link) f(!(!n).value, x)
    else loop(n, f(!(!n).value, x), f)
  }

  def foreach[U](f: T => U): Unit = {
    var n = h.link.next
    while (n != h.link) {
      f(!(!n.asInstanceOf[Ptr[CList[T]]]).value)
      n = n.next
    }
  }
  def foreach_back[U](f: T => U): Unit = {
    var p = h.link.prev
    while (p != h.link) {
      f(!(!p.asInstanceOf[Ptr[CList[T]]]).value)
      p = p.prev
    }
  }
  def size: CSize = {
    var s: CSize = 0.toULong
    foreach(_ => s += 1.toULong)
    s
  }
  @showAsInfix
  @targetName("::")
  def ::(l: CList[T]): CList[T] = {
    h.link.con_head(l.link.next, l.link.prev)
    l.reset()
    h
  }

  def map[T1: Tag](f: T => T1): CList[T1] = {
    import CList.head
    import CList.make
    val h = summon[CList[T1]]
    foreach(x => make(f(x))(summon[Tag[T1]], h))
    h
  }
  def fold[T1](t1: T1, f: (T, T1) => T1): T1 = loop(h, t1, f)
}
