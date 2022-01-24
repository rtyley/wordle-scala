package com.madgag.wordle

import com.madgag.wordle.WordFeedback.CompleteSuccess
import org.roaringbitmap.RoaringBitmap
import CandidateAssay.*
import com.madgag.wordle.PossibleWordSetStore.{idFor, wordSetFor}

import scala.collection.immutable.BitSet

case class CandidateAssay(possibleActualWordsByFeedback: Map[WordFeedback,WordSetId]) {
  require(possibleActualWordsByFeedback.nonEmpty) // there must be _some_ feedback possible, for any candidate!
  private lazy val wordSets: Iterable[BitSet] = possibleActualWordsByFeedback.values.map(wordSetFor)
  require(wordSets.forall(_.nonEmpty)) // do not include map entries for feedback that cannot occur

  lazy val mustBeCorrect: Boolean = possibleActualWordsByFeedback.keySet == OnlyCompleteSuccess
  lazy val canNotBeCorrectAndWouldRevealNoInformation: Boolean =
    possibleActualWordsByFeedback.size == 1 && !mustBeCorrect

  lazy val maxPossibleWordsSize: Int = wordSets.map(_.size).max

  lazy val score: Int = wordSets.map { bitMap =>
    val cardinality = bitMap.size
    cardinality * cardinality
  }.sum

  def updateGiven(newSubSetOfPossibleWords: BitSet): CandidateAssay = CandidateAssay(
    possibleActualWordsByFeedback.view.mapValues {
      originalWordPossibleGivenFeedback
      => wordSetFor(originalWordPossibleGivenFeedback) & newSubSetOfPossibleWords
    }.filter(p => p._2.nonEmpty).mapValues(idFor).toMap
  )

  def summariseFor(corpus: Corpus): String = (for (
    (feedback, bitmap) <- possibleActualWordsByFeedback.toSeq.sortBy(p => wordSetFor(p._2).size)
  ) yield s"${feedback.emojis}:${corpus.humanReadable(wordSetFor(bitmap))}").mkString("  ")
}

object CandidateAssay {
  val OnlyCompleteSuccess = Set(CompleteSuccess)
}

