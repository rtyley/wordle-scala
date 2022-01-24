package com.madgag.wordle

import com.google.common.collect.{BiMap, HashBiMap, Maps}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.SortedSet
import scala.collection.immutable.BitSet

class WordSetId(value: Int) extends AnyVal

object PossibleWordSetStore {

  private val idGen: AtomicInteger = new AtomicInteger()

  private val setById: BiMap[WordSetId, BitSet] =
    Maps.synchronizedBiMap(HashBiMap.create[WordSetId, BitSet]())

  def idFor(possibleWords: BitSet): WordSetId =
    setById.inverse().computeIfAbsent(possibleWords, _ => new WordSetId(idGen.incrementAndGet()))

  def wordSetFor(pwsId: WordSetId): BitSet = setById.get(pwsId)
}
