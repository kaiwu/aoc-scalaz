package aoc.util

import scala.scalanative.unsafe.{CSize, CUnsignedLongLong, Ptr, extern}
import scala.scalanative.unsigned.ULong

@extern
object wyhash {
  def make_secret(seed: CUnsignedLongLong, secret: Ptr[CUnsignedLongLong]): Unit                         = extern
  def wyhash(key: Ptr[Byte], len: CSize, seed: ULong, secret: Ptr[CUnsignedLongLong]): CUnsignedLongLong = extern
}

@extern
object hashmap {}
