package aoc

import scala.scalanative.unsafe._
import scala.scalanative.libc._
import scala.scalanative.unsigned._
import scalaz._
import Scalaz._

@extern
object wyhash {
  def make_secret(seed: CUnsignedLongLong, secret: Ptr[CUnsignedLongLong]): Unit = extern
  def wyhash(key: Ptr[Byte], len: CSize, seed: ULong, secret: Ptr[CUnsignedLongLong]): CUnsignedLongLong = extern
}

type Span[T] = CStruct2[Ptr[T], CSize]
type Array5[T] = CArray[T, Nat._5]
type PPtr[T] = Ptr[Ptr[T]]

object common {
  def get_number(pptr: PPtr[CChar], p: Ptr[CInt]): Unit = {
    var ptr: Ptr[CChar] = !pptr
    !p = 0
    while (!ptr >= '0' && !ptr <= '9') {
      !p = !p * 10 + !ptr - '0'
      ptr += 1
    }
    !pptr = ptr
  }
}