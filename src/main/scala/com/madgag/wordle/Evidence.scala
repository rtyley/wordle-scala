package com.madgag.wordle

import com.madgag.wordle.LetterFeedback.Correct
import com.madgag.wordle.WordFeedback.feedbackFor
import com.madgag.scala.collection.decorators.*
import com.madgag.wordle.Evidence.Summary
import com.madgag.wordle.Wordle.WordIndices

import scala.collection.immutable.SortedMap

case class Evidence(word: Word, wordFeedback: WordFeedback) {
  val ansiColouredString: fansi.Str = (for ((attr, char) <- wordFeedback.toSeq.map(_.ansiColor).zip(word)) yield {
    attr(char.toString)
  }).reduce(_ ++ _)
  
  val plainText: String = word+"-"+wordFeedback.characters

  val isSuccess: Boolean = wordFeedback.isSuccess

  lazy val summary: Summary = {
    val (misplacedIndicies, knownCorrectIndices) = wordFeedback.misplacedAndCorrectIndicies
    Summary(
      misplacedLetters = misplacedIndicies.map(word).letterFrequency,
      correctLettersByIndex = SortedMap.from(knownCorrectIndices.map(index => index -> word(index)))
    )
  }

  override val toString: String = ansiColouredString.toString
}

object Evidence {
  def evidenceFrom(candidate: Word, actual: Word) = Evidence(candidate, feedbackFor(candidate, actual))

  extension (word: Word)
    def compliesWith(evidence: Evidence): Boolean = feedbackFor(evidence.word, word) == evidence.wordFeedback


  case class Summary(misplacedLetters: Map[Letter, Int], correctLettersByIndex: SortedMap[Int, Letter]) {
    val indicesWhichAreNotCorrect: Seq[Int] = WordIndices.filter(!correctLettersByIndex.contains(_))
  }
}