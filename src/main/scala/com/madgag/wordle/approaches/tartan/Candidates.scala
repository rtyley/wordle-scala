package com.madgag.wordle.approaches.tartan

import scala.collection.immutable.SortedSet
import com.madgag.wordle.*
import com.madgag.wordle.wordsets.WordSet

import scala.math.Ordering

case class Candidates(
  possibleWords: WordSet,
  discriminators: WordSet
) {
  def contains(word: WordId) = possibleWords.contains(word) || discriminators.contains(word)

  def allWords: Iterable[WordId] = possibleWords.view ++ discriminators
}
