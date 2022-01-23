package com.madgag.wordle

import com.madgag.wordle.WordFeedback.CompleteSuccess
import org.roaringbitmap.RoaringBitmap
import CandidateAssay._

case class CandidateAssay(possibleActualWordsByFeedback: Map[WordFeedback,RoaringBitmap]) {
  require(possibleActualWordsByFeedback.nonEmpty) // there must be _some_ feedback possible, for any candidate!
  require(possibleActualWordsByFeedback.values.forall(!_.isEmpty)) // do not include map entries for feedback that cannot occur

  lazy val mustBeCorrect: Boolean = possibleActualWordsByFeedback.keySet == OnlyCompleteSuccess
  lazy val canNotBeCorrectAndWouldRevealNoInformation: Boolean =
    possibleActualWordsByFeedback.size == 1 && !mustBeCorrect

  lazy val score: Int = possibleActualWordsByFeedback.values.map { bitMap =>
    val cardinality = bitMap.getCardinality
    cardinality * cardinality
  }.sum

  def updateGiven(newSubSetOfPossibleWords: RoaringBitmap): CandidateAssay = CandidateAssay(
    possibleActualWordsByFeedback.view.mapValues {
      originalWordPossibleGivenFeedback
      => RoaringBitmap.and(originalWordPossibleGivenFeedback,newSubSetOfPossibleWords)
    }.filter(p => !p._2.isEmpty).toMap
  )

  def summariseFor(corpus: Corpus): String = (for (
    (feedback, bitmap) <- possibleActualWordsByFeedback.toSeq.sortBy(_._2.getCardinality)
  ) yield s"${feedback.emojis}:${corpus.humanReadable(bitmap)}").mkString("  ")

  lazy val totalBitMapSize: Long = possibleActualWordsByFeedback.values.map(_.getSizeInBytes).sum
}

object CandidateAssay {
  val OnlyCompleteSuccess = Set(CompleteSuccess)
}

