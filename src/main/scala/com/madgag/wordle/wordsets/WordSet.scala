package com.madgag.wordle.wordsets

import com.madgag.wordle.*

import java.util.concurrent.ConcurrentHashMap
import scala.Array.emptyShortArray
import scala.collection.BitSetOps.WordLength
import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.{AbstractSet, BitSet, SortedSet, SortedSetOps, StrictOptimizedSortedSetOps}
import scala.collection.mutable.{BitSet, Builder}
import scala.collection.{IterableOnce, SpecificIterableFactory}

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

  def wordSetFactory = WordSet

  override val hashCode: Int = super.hashCode() // we rely on the hashcode a lot for `Set`s etc

//  override def equals(that: Any): Boolean = that match {
//    case w: WordSet if w.hashCode == hashCode && w.size == w.size => true // TODO Hack much?
//    case _ => super.equals(that)
//  }
}

object WordSet extends SpecificIterableFactory[WordId, WordSet] {

  val vorgo: ConcurrentHashMap[WordSet, WordSet] = new ConcurrentHashMap[WordSet, WordSet]

  private def intern(ws: WordSet): WordSet = {
    // intern here...
    vorgo.putIfAbsent(ws, ws)
    vorgo.get(ws)
    ws
  }

  // Members declared in scala.collection.Factory
  def fromSpecific(it: IterableOnce[WordId]): WordSet = it match {
    case ws: WordSet => ws
    case s: Set[WordId] =>
      if (s.isEmpty) WordSet.empty else {
        intern(new ShortArrayWordSet(s match {
          case ss: SortedSet[WordId] => ss.toArray
          case _ =>
            val array = s.toArray
            java.util.Arrays.sort(array)
            array
        }))
      }
    case _ =>
      val array = it.iterator.distinct.toArray
      java.util.Arrays.sort(array)
      intern(new ShortArrayWordSet(array))
  }

  def fromKnownDistinct(it: IterableOnce[WordId]): WordSet = {
    val array = it.iterator.toArray
    java.util.Arrays.sort(array)
    fromSpecific(new ShortArrayWordSet(array))
  }




  // also declared in scala.collection.SpecificIterableFactory
  def newBuilder: scala.collection.mutable.Builder[WordId, WordSet] = new scala.collection.mutable.Builder[WordId, WordSet] {
    val builder = SortedSet.newBuilder[Short]

    override def clear(): Unit = builder.clear()

    override def addOne(elem: WordId): this.type = {
      builder.addOne(elem)
      this
    }

    override def result(): WordSet = WordSet.fromSpecific(builder.result())
  }

  final val empty: WordSet = ShortArrayWordSet.empty

}


