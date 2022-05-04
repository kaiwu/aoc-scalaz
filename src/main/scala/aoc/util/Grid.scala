package aoc.util

import aoc.util.Span
import aoc.util.Allocator
import scala.scalanative.libc._
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scala.language.implicitConversions

final case class Grid[T: Tag](width: CInt, height: CInt) {
  val store: Span[T] = {
    val size: CSize = width.toULong * height.toULong
    val p           = summon[Allocator[T]].alloc(size)
    Span.make(p, size)
  }

  type coordinate = (CInt, CInt)
  def apply(c: coordinate): Option[T] = c match {
    case (i, _) if i < 0 || i >= width  => None
    case (_, j) if j < 0 || j >= height => None
    case (i, j)                         => Some(store((j * width + i).toULong))
  }
  def update(c: coordinate, v: T): Unit = c match {
    case (i, _) if i < 0 || i >= width  =>
    case (_, j) if j < 0 || j >= height =>
    case (i, j)                         => !store.at((j * width + i).toULong) = v
  }
  def left(c: coordinate, d: CInt = 1): coordinate  = (c._1 - d, c._2)
  def right(c: coordinate, d: CInt = 1): coordinate = (c._1 + d, c._2)
  def up(c: coordinate, d: CInt = 1): coordinate    = (c._1, c._2 - d)
  def down(c: coordinate, d: CInt = 1): coordinate  = (c._1, c._2 + d)

  def move(c: coordinate, d: CInt, f: (coordinate, CInt) => coordinate): Option[Ptr[T]] = f(c, d) match {
    case (i, _) if i < 0 || i >= width  => None
    case (_, j) if j < 0 || j >= height => None
    case (i, j)                         => Some(store.at((j * width + i).toULong))
  }
  def foreach[U](f: T => U): Unit           = store.foreach(f)
  def fold[T1](t: T1, f: (T, T1) => T1): T1 = store.fold(t, f)
}
