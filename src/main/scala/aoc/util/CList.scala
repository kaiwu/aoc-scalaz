package aoc.util

import scala.Conversion
import scala.language.implicitConversions
import scala.scalanative.unsafe.CStruct2
import scala.scalanative.unsafe.{Tag, CInt, CSize, CStruct1, CUnsignedLongLong, Ptr, extern, name, link}
import scala.scalanative.unsigned.ULong

type DoubleLink = CStruct2[Ptr[Byte], Ptr[Byte]]
type SingleLink = CStruct1[Ptr[Byte]]
type SingleHead = CStruct2[SingleLink, Ptr[SingleLink]]

type CList[T] = CStruct2[DoubleLink, T]

final case class DoubleLinkOps(d: DoubleLink) {
  def prev: Ptr[DoubleLink] = d._1.asInstanceOf[Ptr[DoubleLink]]
  def next: Ptr[DoubleLink] = d._2.asInstanceOf[Ptr[DoubleLink]]
}
given Conversion[DoubleLink, DoubleLinkOps] = DoubleLinkOps(_)

final case class CListOps[T: Tag](p: Ptr[CList[T]]) {
  def link: Ptr[DoubleLink] = p.at1
}
given [T: Tag]: Conversion[Ptr[CList[T]], CListOps[T]] = new CListOps[T](_)

@extern
object clist {
  @name("INIT_LIST_HEAD")
  def init_list_head(head: Ptr[DoubleLink]): Unit = extern

  def list_add(node: Ptr[DoubleLink], head: Ptr[DoubleLink]): Unit         = extern
  def list_add_tail(node: Ptr[DoubleLink], head: Ptr[DoubleLink]): Unit    = extern
  def list_del(node: Ptr[DoubleLink]): Unit                                = extern
  def list_move(list: Ptr[DoubleLink], head: Ptr[DoubleLink]): Unit        = extern
  def list_move_tail(list: Ptr[DoubleLink], head: Ptr[DoubleLink]): Unit   = extern
  def list_empty(list: Ptr[DoubleLink]): CInt                              = extern
  def list_splice(list: Ptr[DoubleLink], head: Ptr[DoubleLink]): Unit      = extern
  def list_splice_init(list: Ptr[DoubleLink], head: Ptr[DoubleLink]): Unit = extern

  @name("INIT_SLIST_HEAD")
  def init_slist_head(list: Ptr[SingleHead]): Unit                                               = extern
  def slist_add_head(node: Ptr[SingleLink], head: Ptr[SingleHead]): Unit                         = extern
  def slist_add_tail(node: Ptr[SingleLink], head: Ptr[SingleHead]): Unit                         = extern
  def slist_add_after(node: Ptr[SingleLink], prev: Ptr[SingleLink], head: Ptr[SingleHead]): Unit = extern
  def slist_del_head(head: Ptr[SingleHead]): Unit                                                = extern
  def slist_del_after(prev: Ptr[SingleLink], head: Ptr[SingleHead]): Unit                        = extern
  def slist_empty(head: Ptr[SingleHead]): CInt                                                   = extern
  def slist_splice(list: Ptr[SingleHead], at: Ptr[SingleLink], head: Ptr[SingleHead]): Unit      = extern
  def slist_splice_init(list: Ptr[SingleHead], at: Ptr[SingleLink], head: Ptr[SingleHead]): Unit = extern
}
