package com.madgag.wordle.approaches.tartan

import scala.collection.immutable.SortedSet
import com.madgag.wordle.*
import com.madgag.wordle.wordsets.WordSet

import java.util.concurrent.atomic.LongAdder
import scala.math.Ordering

object Candidates {
  val creationCounter = new LongAdder
  val all: scala.collection.mutable.Set[Candidates] = scala.collection.mutable.Set()
}

case class Candidates(
  possibleWords: WordSet,
  discriminators: WordSet
) {
  Candidates.creationCounter.increment()
  Candidates.all.addOne(this)


  def contains(word: WordId) = possibleWords.contains(word) || discriminators.contains(word)

  def allWords: Iterable[WordId] = possibleWords.view ++ discriminators
}
