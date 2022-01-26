package com.madgag.wordle

import com.madgag.wordle.WordFeedback.CompleteSuccess
import CandidateAssay.*

import scala.collection.immutable.BitSet

case class CandidateAssay(possibleActualWordsByFeedback: Map[WordFeedback, WordSet]) {
  require(possibleActualWordsByFeedback.nonEmpty) // there must be _some_ feedback possible, for any candidate!
  private lazy val wordSets: Iterable[BitSet] = possibleActualWordsByFeedback.values.map(_.bitSet)
  require(wordSets.forall(_.nonEmpty)) // do not include map entries for feedback that cannot occur

  lazy val mustBeCorrect: Boolean = possibleActualWordsByFeedback.keySet == OnlyCompleteSuccess
  lazy val canNotBeCorrectAndWouldRevealNoInformation: Boolean =
    possibleActualWordsByFeedback.size == 1 && !mustBeCorrect

  lazy val maxPossibleWordsSize: Int = wordSets.map(_.size).max

  lazy val score: Int = wordSets.map { bitMap =>
    val cardinality = bitMap.size
    cardinality * cardinality
  }.sum

  def updateGiven(idForNewSubsetOfPossibleWords: WordSet): CandidateAssay = CandidateAssay(
    possibleActualWordsByFeedback.view.mapValues {
      originalWordPossibleGivenFeedback
      => PossibleWordSetStore.intersect(originalWordPossibleGivenFeedback, idForNewSubsetOfPossibleWords)
    }.filter(p => p._2 != PossibleWordSetStore.emptySet).toMap
  )

  def summariseFor(corpus: Corpus): String = (for (
    (feedback, wordSet) <- possibleActualWordsByFeedback.toSeq.sortBy(p => p._2.bitSet.size)
  ) yield s"${feedback.emojis}:${corpus.humanReadable(wordSet.bitSet)}").mkString("  ")
}

object CandidateAssay {
  val OnlyCompleteSuccess = Set(CompleteSuccess)
}

