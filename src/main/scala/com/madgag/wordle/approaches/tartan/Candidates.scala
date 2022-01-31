package com.madgag.wordle.approaches.tartan

import scala.collection.immutable.SortedSet
import com.madgag.wordle.*

case class Candidates(
  possibleWords: SortedSet[WordId],
  discriminators: SortedSet[WordId]
) {
  def allWords: IndexedSeq[WordId] = (possibleWords ++ discriminators).toIndexedSeq
}
