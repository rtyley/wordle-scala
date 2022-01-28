package com.madgag.wordle

import com.madgag.wordle.WordFeedback.CompleteSuccess
import CandidateAssay.*

import scala.collection.immutable.BitSet

case class CandidateAssay(possibleWordsByFeedback: Map[WordFeedback, WordSet]) {
  require(possibleWordsByFeedback.nonEmpty) // there must be _some_ feedback possible, for any candidate!
  private lazy val wordSets: Iterable[BitSet] = possibleWordsByFeedback.values.map(_.bitSet)
  require(wordSets.forall(_.nonEmpty)) // do not include map entries for feedback that cannot occur

  lazy val mustBeCorrect: Boolean = possibleWordsByFeedback.keySet == OnlyCompleteSuccess
  lazy val canNotBeCorrectAndWouldRevealNoInformation: Boolean =
    possibleWordsByFeedback.size == 1 && !mustBeCorrect

  lazy val maxPossibleWordsSize: Int = wordSets.map(_.size).max

  lazy val score: Int = wordSets.map { bitMap =>
    val cardinality = bitMap.size
    cardinality * cardinality
  }.sum

  def updateGiven(idForNewSubsetOfPossibleWords: WordSet): CandidateAssay = CandidateAssay(
    possibleWordsByFeedback.view.mapValues {
      originalWordPossibleGivenFeedback
      => PossibleWordSetStore.intersect(originalWordPossibleGivenFeedback, idForNewSubsetOfPossibleWords)
    }.filter(p => p._2 != PossibleWordSetStore.emptySet).toMap
  )
}

object CandidateAssay {
  val OnlyCompleteSuccess = Set(CompleteSuccess)
}

