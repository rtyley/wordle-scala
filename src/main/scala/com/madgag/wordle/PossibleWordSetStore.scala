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