package com.madgag.wordle.wordsets

import com.madgag.wordle.*

import scala.Array.emptyShortArray
import scala.collection.BitSetOps.WordLength
import scala.collection.{IterableOnce, SpecificIterableFactory}
import scala.collection.Stepper.EfficientSplit
import scala.collection.mutable.{BitSet, Builder}
// import scala.collection.{AbstractIterator, AnyStepper, BitSet, BitSetOps, IterableOnce, Iterator, Set, SortedSet, SortedSetOps, SpecificIterableFactory, Stepper, StepperShape, View}
import scala.collection.immutable.{AbstractSet, BitSet, SortedSet, SortedSetOps, StrictOptimizedSortedSetOps}

/** Base implementation type of WordSets */
trait WordSetOps[+C <: WordSet with WordSetOps[C]]
  extends SortedSetOps[WordId, SortedSet, C] { self =>
//  import BitSetOps._

  // def wordSetFactory: SpecificIterableFactory[WordId, C]

  def unsorted: Set[WordId]

  final def ordering: Ordering[WordId] = Ordering.Short
}

abstract class WordSet
  extends AbstractSet[WordId]
    with SortedSet[WordId]
    with SortedSetOps[WordId, SortedSet, WordSet]
    with StrictOptimizedSortedSetOps[WordId, SortedSet, WordSet]
    // with collection.BitSet
    with WordSetOps[WordSet]
    with Serializable {

  override protected def fromSpecific(coll: IterableOnce[WordId]): WordSet = wordSetFactory.fromSpecific(coll)
  override protected def newSpecificBuilder: Builder[WordId, WordSet] = wordSetFactory.newBuilder
  override def empty: WordSet = wordSetFactory.empty

  def wordSetFactory = ShortArrayWordSet

}

object WordSet {
  final val empty: WordSet = ShortArrayWordSet.empty

  // Member declared in scala.collection.Factory
  def fromSpecific(it: IterableOnce[WordId]): WordSet = ShortArrayWordSet.fromSpecific(it)
}


