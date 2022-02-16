package com.madgag.wordle.wordsets

import scala.collection.{IterableOnce, SpecificIterableFactory}
import com.madgag.wordle.WordId
import com.madgag.wordle.wordsets.WordSet.intern

import java.util
import scala.Array.emptyShortArray
import scala.collection.immutable.SortedSet
import scala.util.Sorting

class ShortArrayWordSet(elems: Array[Short]) extends WordSet {
  // require(elems.length < 2 || elems.sliding(2).forall { a => a(0) <= a(1) })


  override def knownSize: Int = elems.length

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

  override def removedAll(it: IterableOnce[WordId]): WordSet = it match {
    case ss: SortedSet[WordId] =>
      val builder = WordSet.newBuilder
      val newElems: Array[Short] = Array.ofDim(elems.length)
      var alreadySearchedUpTo: Int = 0
      var oldElemsCopiedUpTo: Int = 0
      var newElemsPopulatedUpTo: Int = 0
      for (elemToRemove <- ss) {
        val elemIndex = util.Arrays.binarySearch(elems, alreadySearchedUpTo, elems.length, elemToRemove) // (-(insertion point) - 1)
        if (elemIndex < 0) {
          alreadySearchedUpTo = -(elemIndex+1)
        } else { // Element to remove has been found
          val chunkSize = elemIndex - oldElemsCopiedUpTo
          Array.copy(elems,oldElemsCopiedUpTo,newElems,newElemsPopulatedUpTo,chunkSize)
          oldElemsCopiedUpTo = elemIndex + 1
          newElemsPopulatedUpTo += chunkSize
          alreadySearchedUpTo = elemIndex + 1
        }
      }
      val finalChunk = elems.length - oldElemsCopiedUpTo
      Array.copy(elems,oldElemsCopiedUpTo,newElems,newElemsPopulatedUpTo,finalChunk)
      val finalElems: Array[Short] = Array.ofDim(newElemsPopulatedUpTo+finalChunk)
      Array.copy(newElems,0,finalElems,0,finalElems.length)
      new ShortArrayWordSet(finalElems)

    case _ => super.removedAll(it)
  }



  // Members declared in scala.collection.SortedOps
  def rangeImpl(from: Option[WordId], until: Option[WordId]): WordSet = ???

  // Members declared in scala.collection.SortedSetOps
  def iteratorFrom(start: WordId): Iterator[WordId] = ???

}


object ShortArrayWordSet {
  // Members declared in scala.collection.SpecificIterableFactory
  final val empty: WordSet = new ShortArrayWordSet(emptyShortArray)
  // def newBuilder: scala.collection.mutable.Builder[WordId, WordSet] = ???
}
