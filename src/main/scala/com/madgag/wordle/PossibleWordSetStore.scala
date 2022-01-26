package com.madgag.wordle

import com.google.common.collect.{BiMap, HashBiMap, Maps}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.SortedSet
import scala.collection.concurrent.TrieMap
import scala.collection.immutable.BitSet

class WordSetId(val value: Int) extends AnyVal

case class WordSet(id: Int, bitSet: BitSet) {
//  def intersect(other: WordSet): WordSet = {
//
//  }
}

object PossibleWordSetStore {

  private val idGen: AtomicInteger = new AtomicInteger()

//  private val setById: BiMap[WordSetId, BitSet] =
//    Maps.synchronizedBiMap(HashBiMap.create[WordSetId, BitSet]())
//
//  def idFor(possibleWords: BitSet): WordSetId =
//    setById.inverse().computeIfAbsent(possibleWords, _ => new WordSetId(idGen.incrementAndGet()))
//
//  def wordSetFor(pwsId: WordSetId): BitSet = setById.get(pwsId)

  def numStoredSets: Int = idGen.get()

  val internMap: TrieMap[Set[Int], WordSet] = TrieMap.empty

  def intern(words: Set[Int]): WordSet = internMap.getOrElseUpdate(words, createOptimisedVersionOf(words))

  private def createOptimisedVersionOf(wordSet: Set[Int]): WordSet =
    WordSet(idGen.incrementAndGet(), BitSet.fromSpecific(wordSet)) // later we may even construct optimised storage types


  val emptySet: WordSet = intern(Set.empty)

  private val intersectionByOperands: TrieMap[Long, WordSet] = scala.collection.concurrent.TrieMap.empty

  def intersect(a: WordSet, b: WordSet): WordSet = {
    val intersectionId = if (a.id > b.id) (a.id << 32) + b.id else (b.id << 32) + a.id
    intersectionByOperands.getOrElseUpdate(intersectionId, intern(a.bitSet & b.bitSet))
  }
}

/*
Why not interning? Well, comparing two BitSets for equality is really slow. Can't use them as keys for Maps easily,
so can't do things like caching the results of intersecting two sets to get a new set.

What if we were interning to a better format, one that was, yes, more compact than a BitSet for our sparse data
BUT ALSO carried INSIDE IT a unique identifier that could be used for instant equality checks?

What if you could compare equality by looking at a single long, rather than all the data? What if the first,
and possibly only, entry in the array-of-longs was the id (allocated from an AtomicInteger for WordSets that
have more than 5 items)?

When Wordset has 6 items - we *must* be able to compare equality JUST by looking at the first entry (and possibly array length?!?).


Or what if we simply use BitSet, but store it in a java.util.IdentityHashMap? How would that work for
caching the results of intersecting two sets to get a new set??


*/
//object WordSetInterning {
//
//  val m: TrieMap[Set[Int], BitSet] = TrieMap.empty
//
//  def intern(foo: Set[Int]): BitSet = m.getOrElseUpdate(foo, optimisedVersionOf(foo))
//
//  def optimisedVersionOf(wordSet: Set[Int]): BitSet = {
//    BitSet.fromSpecific(wordSet) // later we may even construct optimised storage types
//  }
//}