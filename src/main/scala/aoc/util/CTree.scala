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

type TreeLink = CStruct2[PPtr[Byte], PPtr[Byte]]
type CTree[T] = CStruct2[TreeLink, T]

object CTree {}

final case class TreeLinkOps(t: Ptr[TreeLink]) {
  def left: Ptr[TreeLink]             = !t.at1.asInstanceOf[PPtr[TreeLink]]
  def right: Ptr[TreeLink]            = !t.at2.asInstanceOf[PPtr[TreeLink]]
  def left_=(p: Ptr[TreeLink]): Unit  = !t.at1.asInstanceOf[PPtr[TreeLink]] = p
  def right_=(p: Ptr[TreeLink]): Unit = !t.at2.asInstanceOf[PPtr[TreeLink]] = p

  def reset(): Unit = { left = t; right = t }
}

given Conversion[Ptr[TreeLink], TreeLinkOps]             = TreeLinkOps(_)
given [T: Tag: Order]: Conversion[CTree[T], CTreeOps[T]] = new CTreeOps[T](_)

final case class CTreeOps[T: Tag: Order](root: CTree[T]) {
  def tlink: Ptr[TreeLink] = root.at1
  def tvalue: Ptr[T]       = root.at2
  def empty: CBool         = tlink.left == tlink && tlink.right == tlink
  def left: CTree[T]       = !tlink.left.asInstanceOf[Ptr[CTree[T]]]
  def right: CTree[T]      = !tlink.right.asInstanceOf[Ptr[CTree[T]]]

  def insert(node: CTree[T]): Unit = summon[Order[T]](!root.tvalue, !node.tvalue) match {
    case Ordering.EQ => !tvalue = !node.tvalue
    case Ordering.LT =>
      if (tlink.left == tlink) tlink.left = node.link
      else left.insert(node)
    case Ordering.GT =>
      if (tlink.right == tlink) tlink.right = node.link
      else right.insert(node)
  }
}
