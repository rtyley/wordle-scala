package com.madgag.wordle

import java.io.{ObjectInputStream, ObjectOutputStream}
import scala.collection.{BitSet, IterableOnce, Set, SpecificIterableFactory, immutable}
import scala.collection.immutable.AbstractSet
import scala.collection.mutable.Builder

class PossibleWordSet(data: Array[Long]) extends AbstractSet[Int] {
  override def incl(elem: Int): Predef.Set[Int] = ???

  override def excl(elem: Int): Predef.Set[Int] = ???

  override def contains(elem: Int): Boolean = ???

  override def iterator: Iterator[Int] = ???
}

object PossibleWordSet extends SpecificIterableFactory[Int, PossibleWordSet] {

  final val empty: PossibleWordSet = new PossibleWordSet(Array.empty)
  def newBuilder: Builder[Int, PossibleWordSet] = PossibleWordSet.newBuilder
  def fromSpecific(it: IterableOnce[Int]): BitSet = {
    it.iterator.grouped(5).map {
      
    }
  }

}