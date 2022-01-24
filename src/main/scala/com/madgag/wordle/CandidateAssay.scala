package com.madgag.wordle

import com.madgag.wordle.WordFeedback.CompleteSuccess
import org.roaringbitmap.RoaringBitmap
import CandidateAssay.*

import scala.collection.immutable.BitSet

case class CandidateAssay(possibleActualWordsByFeedback: Map[WordFeedback,BitSet]) {
  require(possibleActualWordsByFeedback.nonEmpty) // there must be _some_ feedback possible, for any candidate!
  require(possibleActualWordsByFeedback.values.forall(!_.isEmpty)) // do not include map entries for feedback that cannot occur

  lazy val mustBeCorrect: Boolean = possibleActualWordsByFeedback.keySet == OnlyCompleteSuccess
  lazy val canNotBeCorrectAndWouldRevealNoInformation: Boolean =
    possibleActualWordsByFeedback.size == 1 && !mustBeCorrect

  lazy val maxPossibleWordsSize: Int = possibleActualWordsByFeedback.values.map(_.size).max

  lazy val score: Int = possibleActualWordsByFeedback.values.map { bitMap =>
    val cardinality = bitMap.size
    cardinality * cardinality
  }.sum

  def updateGiven(newSubSetOfPossibleWords: BitSet): CandidateAssay = CandidateAssay(
    possibleActualWordsByFeedback.view.mapValues {
      originalWordPossibleGivenFeedback
      => originalWordPossibleGivenFeedback & newSubSetOfPossibleWords
    }.filter(p => p._2.nonEmpty).toMap
  )

  def summariseFor(corpus: Corpus): String = (for (
    (feedback, bitmap) <- possibleActualWordsByFeedback.toSeq.sortBy(_._2.size)
  ) yield s"${feedback.emojis}:${corpus.humanReadable(bitmap)}").mkString("  ")
}

object CandidateAssay {
  val OnlyCompleteSuccess = Set(CompleteSuccess)
}

