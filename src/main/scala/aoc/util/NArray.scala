package aoc
package util

import scala.scalanative.unsafe._
import scala.scalanative.libc._
import scala.scalanative.unsigned._
import scala.annotation.tailrec
import scalaz._
import Scalaz._

type NArray[T, N <: Nat] = CArray[T, N]

object NArray {
  def apply[T: Tag, N <: Nat](args: T*)(using ptr: Ptr[CArray[T, N]], tag: Tag[CArray[T, N]]): Ptr[CArray[T, N]] = {
    var index: CSize = 0.toULong
    val total: CSize = tag.size / sizeof[T]
    string.memset(ptr.asInstanceOf[Ptr[Byte]], 0x0, tag.size)
    args.toSeq.foreach((x: T) => {
      if (index < total) {
        ptr.asInstanceOf[Ptr[T]](index) = x
        index += 1.toULong
      }
    })
    ptr
  }
}
