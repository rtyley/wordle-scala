package com.madgag.wordle

import java.io.{ObjectInputStream, ObjectOutputStream}
import scala.collection.BitSetOps.WordLength
import scala.collection.{AbstractIterator, BitSet, IterableOnce, Iterator, Set, SortedSet, SpecificIterableFactory, immutable}
import scala.collection.immutable.AbstractSet
import scala.collection.mutable.Builder

class PossibleWordSet(data: Array[Long]) extends AbstractSet[Int] {

  override def knownSize: Int =
    ((data(0) >>> 61).toInt & 0x7) + (if (data.length > 1) data.length * 5 else 0)

  override def incl(elem: Int): Predef.Set[Int] = ???

  override def excl(elem: Int): Predef.Set[Int] = ???

  override def contains(elem: Int): Boolean = {
    val index: Int = ??? // data.search(elem)((x: Long, y: Long) => ((x & 0xFFF) - (y & 0xFFF)).toInt).insertionPoint
    elementsAtDataIndex(index).contains(elem)
  }

  private def elementsAtDataIndex(index: Int): Iterator[Int] = ???

  override def iterator: Iterator[Int] = new AbstractIterator[Int] {
    private[this] var currentPos = 0
    final override def hasNext: Boolean = {
//
//      while (currentWord == 0) {
//        if (currentPos + 1 >= nwords) return false
//        currentPos += 1
//        currentWord = word(currentPos)
//      }
      true
    }
    final override def next(): Int = {
      if (hasNext) {
        (data(currentPos / 5) >>> (currentPos % 5)).toInt & 0xFFF
      } else Iterator.empty.next()
    }
  }
}

//object PossibleWordSet extends SpecificIterableFactory[Int, PossibleWordSet] {
//
//  final val empty: PossibleWordSet = new PossibleWordSet(Array.empty)
//  def newBuilder: Builder[Int, PossibleWordSet] = PossibleWordSet.newBuilder
//  def fromSpecific(it: IterableOnce[Int]): PossibleWordSet = {
//    val items = SortedSet.from(it)
//    new PossibleWordSet(Array.from(items.grouped(5).map { g =>
//      g.foldLeft(0) {
//        case (total, wordId) => (total << 12) + wordId
//      }
//    }))
//  }
//
//}