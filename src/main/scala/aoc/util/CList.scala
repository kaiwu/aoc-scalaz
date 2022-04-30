package aoc.util

import scala.scalanative.unsafe.Tag.CStruct2
import scala.scalanative.unsafe.{CSize, CStruct1, CUnsignedLongLong, Ptr, extern, name}
import scala.scalanative.unsigned.ULong

type DoubleLink = CStruct2[Ptr[Byte], Ptr[Byte]]
type SingleLink = CStruct1[Ptr[Byte]]

@extern
object CList {
  @name("INIT_LIST_HEAD")
  def init_list_head(head: Ptr[DoubleLink]): Unit                  = extern
  def list_add(node: Ptr[DoubleLink], head: Ptr[DoubleLink]): Unit = extern
}
