package aoc

import scala.scalanative.unsafe._
import scala.scalanative.libc._
import scalaz._
import Scalaz._

type Span[T] = CStruct2[Ptr[T], CSize]
type Array5[T] = CArray[T, Nat._5]
type PPtr[T] = Ptr[Ptr[T]]