package com.madgag.wordle.approaches.tartan

import scala.collection.immutable.SortedSet

case class Candidates(
  possibleWords: SortedSet[Int],
  discriminators: SortedSet[Int]
) {
  def allWords: IndexedSeq[Int] = (possibleWords ++ discriminators).toIndexedSeq
}
