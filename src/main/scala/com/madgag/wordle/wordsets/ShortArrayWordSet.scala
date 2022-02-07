package com.madgag.wordle.wordsets

import scala.collection.SpecificIterableFactory
import com.madgag.wordle.WordId

import java.util
import scala.Array.emptyShortArray
import scala.collection.immutable.SortedSet
import scala.util.Sorting

class ShortArrayWordSet(elems: Array[Short]) extends WordSet {
  require(elems.length < 2 || elems.sliding(2).forall { a => a(0) <= a(1) })

  // Members declared in scala.collection.IterableOnce
  def iterator: Iterator[WordId] = elems.iterator

  // Members declared in scala.collection.SetOps
  def contains(elem: WordId): Boolean = elems.contains(elem)

  // Members declared in scala.collection.immutable.SetOps
  def excl(elem: WordId): WordSet = {
    val elemIndex = util.Arrays.binarySearch(elems, elem) // (-(insertion point) - 1)
    if (elemIndex<0) this else {
      val newElems: Array[Short] = Array.ofDim(elems.length - 1)
      Array.copy(elems,0,newElems,0,elemIndex)
      Array.copy(elems,elemIndex+1,newElems,elemIndex,elems.length-elemIndex-1)
      new ShortArrayWordSet(newElems)
    }
  }
  def incl(elem: WordId): WordSet = {
    val negInsert = util.Arrays.binarySearch(elems, elem) // (-(insertion point) - 1)
    if (negInsert>=0) this else {
      val elemIndex = -negInsert - 1
      val newElems: Array[Short] = Array.ofDim(elems.length + 1)
      Array.copy(elems,0,newElems,0,elemIndex)
      newElems(elemIndex) = elem
      Array.copy(elems,elemIndex,newElems,-negInsert,elems.length-elemIndex)
      new ShortArrayWordSet(newElems)
    }
  }

  // Members declared in scala.collection.SortedOps
  def rangeImpl(from: Option[WordId], until: Option[WordId]): WordSet = ???

  // Members declared in scala.collection.SortedSetOps
  def iteratorFrom(start: WordId): Iterator[WordId] = ???

}


object ShortArrayWordSet extends SpecificIterableFactory[WordId, WordSet] {

  // Members declared in scala.collection.Factory
  def fromSpecific(it: IterableOnce[WordId]): WordSet = it match {
    case ws: WordSet => ws
    case s: Set[WordId] =>
      new ShortArrayWordSet(s match {
        case ss: SortedSet[WordId] => ss.toArray
        case _ =>
          val a = s.toArray
          java.util.Arrays.sort(a)
          a
      })
    case _ => new ShortArrayWordSet(SortedSet.from(it).toArray)
  }

  // also declared in scala.collection.SpecificIterableFactory
  def newBuilder: scala.collection.mutable.Builder[WordId, WordSet] = new scala.collection.mutable.Builder[WordId, WordSet] {
    val builder = SortedSet.newBuilder[Short]

    override def clear(): Unit = builder.clear()

    override def addOne(elem: WordId): this.type = {
      builder.addOne(elem)
      this
    }

    override def result(): WordSet = new ShortArrayWordSet(builder.result().toArray)
  }

  // Members declared in scala.collection.SpecificIterableFactory
  final val empty: WordSet = new ShortArrayWordSet(emptyShortArray)
  // def newBuilder: scala.collection.mutable.Builder[WordId, WordSet] = ???
}
