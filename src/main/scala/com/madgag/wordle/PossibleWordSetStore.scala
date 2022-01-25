package com.madgag.wordle

import com.google.common.collect.{BiMap, HashBiMap, Maps}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.SortedSet
import scala.collection.concurrent.TrieMap
import scala.collection.immutable.BitSet

class WordSetId(val value: Int) extends AnyVal

object PossibleWordSetStore {

  private val idGen: AtomicInteger = new AtomicInteger()

  private val setById: BiMap[WordSetId, BitSet] =
    Maps.synchronizedBiMap(HashBiMap.create[WordSetId, BitSet]())

  def idFor(possibleWords: BitSet): WordSetId =
    setById.inverse().computeIfAbsent(possibleWords, _ => new WordSetId(idGen.incrementAndGet()))

  def wordSetFor(pwsId: WordSetId): BitSet = setById.get(pwsId)

  def numStoredSets: Int = idGen.get()

  val idForEmpty: WordSetId = idFor(BitSet.empty)

  private val intersectionByOperands: TrieMap[(WordSetId,WordSetId), WordSetId] = scala.collection.concurrent.TrieMap.empty

  def intersect(a: WordSetId, b: WordSetId): WordSetId = {
    idFor(wordSetFor(a) & wordSetFor(b))
//    val key = if (a.value < b.value) (a, b) else (b, a)
//    intersectionByOperands.getOrElseUpdate(key, idFor(wordSetFor(a) & wordSetFor(b)))
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