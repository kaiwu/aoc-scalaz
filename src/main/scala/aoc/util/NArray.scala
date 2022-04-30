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
  def apply[T: Tag, N <: Nat](args: T*)(using a: Ptr[CArray[T, N]], tag: Tag[CArray[T, N]]): CArray[T, N] = {
    var index: CInt = 0
    val total: CInt = (tag.size / sizeof[T]).toInt
    string.memset(a.at(0).asInstanceOf[Ptr[Byte]], 0x0, tag.size)
    args.toSeq.foreach((x: T) => {
      if (index < total) {
        a(index) = x
        index += 1
      }
    })
    a
  }
}
