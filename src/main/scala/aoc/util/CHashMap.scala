package aoc.util

import scalaz.*
import Scalaz.*
import scalaz.Ordering.{EQ, LT, GT}

import scala.Conversion
import scala.language.implicitConversions
import scala.scalanative.unsafe.*
import scala.scalanative.libc.*
import scala.scalanative.unsigned.*

@extern
object wyhash {
  def make_secret(seed: CUnsignedLongLong, secret: Ptr[CUnsignedLongLong]): Unit                         = extern
  def wyhash(key: Ptr[Byte], len: CSize, seed: ULong, secret: Ptr[CUnsignedLongLong]): CUnsignedLongLong = extern
}

type malloc_t   = CFuncPtr1[CSize, Ptr[Byte]]
type remalloc_t = CFuncPtr2[Ptr[Byte], CSize, Ptr[Byte]]
type free_t     = CFuncPtr1[Ptr[Byte], Unit]
type hash_t     = CFuncPtr3[Ptr[Byte], CUnsignedLongLong, CUnsignedLongLong, CUnsignedLongLong]
type compare_t  = CFuncPtr3[Ptr[Byte], Ptr[Byte], Ptr[Byte], CInt]
type void_t     = Ptr[Byte]
type iter_t     = CFuncPtr2[void_t, void_t, CBool]
type hashmap = CStruct21[
  malloc_t,
  remalloc_t,
  free_t,
  CBool,
  CSize,
  CSize,
  CUnsignedLongLong,
  CUnsignedLongLong,
  hash_t,
  compare_t,
  free_t,
  void_t,
  CSize,
  CSize,
  CSize,
  CSize,
  CSize,
  CSize,
  void_t,
  void_t,
  void_t
]

@extern
object hashmap {
  def hashmap_new(s: CSize, c: CSize, s0: CSize, s1: CSize, h: hash_t, co: compare_t, f: free_t, u: void_t): Ptr[hashmap] = extern
  def hashmap_free(p: Ptr[hashmap]): Unit                                                                                 = extern
  def hashmap_clear(p: Ptr[hashmap], b: CBool): Unit                                                                      = extern
  def hashmap_count(p: Ptr[hashmap]): CSize                                                                               = extern
  def hashmap_get(p: Ptr[hashmap], item: void_t): void_t                                                                  = extern
  def hashmap_set(p: Ptr[hashmap], item: void_t): void_t                                                                  = extern
  def hashmap_delete(p: Ptr[hashmap], item: void_t): void_t                                                               = extern
  def hashmap_scan(p: Ptr[hashmap], iter: iter_t, u: void_t): CBool                                                       = extern
  def hashmap_iter(p: Ptr[hashmap], i: Ptr[CSize], iterm: Ptr[Ptr[Byte]]): CBool                                          = extern
  def hashmap_sip(d: void_t, l: CSize, s0: CUnsignedLongLong, s1: CUnsignedLongLong): CUnsignedLongLong                   = extern
  def hashmap_murmur(d: void_t, l: CSize, s0: CUnsignedLongLong, s1: CUnsignedLongLong): CUnsignedLongLong                = extern
}

final case class CHashMap[K: Order: Tag, V: Tag](map: Ptr[hashmap])
object CHashMap {
  def apply[K: Order: Tag, V: Tag](): CHashMap[K, V] = {
    val compare: compare_t = (a: void_t, b: void_t, c: void_t) => {
      val k1      = a.asInstanceOf[Ptr[K]]
      val k2      = b.asInstanceOf[Ptr[K]]
      val compare = summon[Order[K]]
      compare.order(!k1, !k2) match {
        case EQ => 0
        case LT => -1
        case GT => 1
      }
    }

    val ksize                    = summon[Tag[K]].size
    val vsize                    = summon[Tag[V]].size
    val cap: CSize               = 1024.toULong
    val seed0: CUnsignedLongLong = 0.toULong
    val seed1: CUnsignedLongLong = 1.toULong
    val hash: hash_t             = hashmap.hashmap_sip(_, ksize, _, _)
    val m                        = hashmap.hashmap_new(ksize + vsize, cap, seed0, seed1, hash, compare, null, null)
    CHashMap(m)
  }
}

final case class CHashMapOps[K: Order: Tag, V: Tag](m: CHashMap[K, V]) {
  type KV[K, V] = CStruct2[K, V]
  def insert(k: K, v: V): Ptr[KV[K, V]] = {
    val kv = stackalloc[KV[K, V]]()
    kv._1 = k
    kv._2 = v
    hashmap.hashmap_set(m.map, kv.asInstanceOf[Ptr[Byte]]).asInstanceOf[Ptr[KV[K, V]]]
  }
  def insert(p: (K, V)): Ptr[KV[K, V]] = insert(p._1, p._2)
  def find(k: K): Ptr[KV[K, V]] = {
    val p = stackalloc[K]()
    !p = k
    hashmap.hashmap_get(m.map, p.asInstanceOf[Ptr[Byte]]).asInstanceOf[Ptr[KV[K, V]]]
  }
  def erase(k: K): Ptr[KV[K, V]] = {
    val p = stackalloc[K]()
    !p = k
    hashmap.hashmap_delete(m.map, p.asInstanceOf[Ptr[Byte]]).asInstanceOf[Ptr[KV[K, V]]]
  }
  def foreach[U](f: Ptr[KV[K, V]] => U): Unit = {
    val iter: iter_t = (a: void_t, b: void_t) => {
      f(a.asInstanceOf[Ptr[KV[K, V]]])
      true
    }
    hashmap.hashmap_scan(m.map, iter, null)
  }
  def clear(): Unit = hashmap.hashmap_clear(m.map, true)
  def size: CSize   = hashmap.hashmap_count(m.map)
}
given [K: Order: Tag, V: Tag]: Conversion[CHashMap[K, V], CHashMapOps[K, V]] = new CHashMapOps[K, V](_)
