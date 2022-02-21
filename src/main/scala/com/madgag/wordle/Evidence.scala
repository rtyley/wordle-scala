package com.madgag.wordle

import com.madgag.wordle.LetterFeedback.Correct
import com.madgag.wordle.WordFeedback.feedbackFor
import com.madgag.scala.collection.decorators.*
import com.madgag.wordle.Evidence.Summary
import com.madgag.wordle.Wordle.WordIndices

import scala.collection.immutable.SortedMap

case class Evidence(guess: Word, wordFeedback: WordFeedback) {
  val ansiColouredString: fansi.Str = (for ((attr, char) <- wordFeedback.toSeq.map(_.ansiColor).zip(guess)) yield {
    attr(char.toString)
  }).reduce(_ ++ _)
  
  val plainText: String = guess+"-"+wordFeedback.characters

  val isSuccess: Boolean = wordFeedback.isSuccess

  lazy val summary: Summary = {
    val (misplacedIndices, knownCorrectIndices) = wordFeedback.misplacedAndCorrectIndices
    Summary(
      misplacedLetters = misplacedIndices.map(guess).letterFrequency,
      correctLettersByIndex = SortedMap.from(knownCorrectIndices.map(index => index -> guess(index)))
    )
  }

  override val toString: String = ansiColouredString.toString
}

object Evidence {
  def evidenceFrom(candidate: Word, actual: Word) = Evidence(candidate, feedbackFor(candidate, actual))

  extension (word: Word)
    def compliesWith(evidence: Evidence): Boolean = feedbackFor(evidence.guess, word) == evidence.wordFeedback


  case class Summary(misplacedLetters: FrequencyMap[Letter], correctLettersByIndex: SortedMap[Int, Letter]) {
    val indicesWhichAreNotCorrect: Seq[Int] = WordIndices.filter(!correctLettersByIndex.contains(_))
  }
}