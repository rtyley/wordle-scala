package com.madgag.wordle

import com.madgag.scala.collection.decorators.*
import com.madgag.wordle.LetterFeedback.*
import com.madgag.wordle.Wordle.{Foo, Word, WordIndices, WordLength}

class WordFeedback(val underlying: Int) extends AnyVal {
  def toSeq: Seq[LetterFeedback] = {
    var u = underlying
    (for (_ <- WordIndices) yield {
      val letterFeedback = LetterFeedback.fromOrdinal(u % WordFeedback.numValues)
      u /= WordFeedback.numValues
      letterFeedback
    }).reverse
  }
  def emojis: String = toSeq.map(_.emoji).mkString

  def isSuccess: Boolean = toSeq.forall(_ == Green)

  override def toString: Word = emojis
}

object WordFeedback {
  val numValues: Int = LetterFeedback.values.length

  def apply(letterFeedbacks: Seq[LetterFeedback]): WordFeedback = {
    require(letterFeedbacks.size == WordLength)
    new WordFeedback(
      letterFeedbacks.foldLeft(0) {
        case (total, letterFeedback) => (total * numValues) + letterFeedback.ordinal
      }
    )
  }

  def feedbackFor(candidate: Word, actual: Word): WordFeedback = {
    val (correctIndices, incorrectIndices) = WordIndices.partition(index => candidate(index) == actual(index))
    val misplacedLetterIndices = incorrectIndices.foldLeft(Foo(remainingActualLetters = incorrectIndices.map(actual).groupUp(identity)(_.size))) {
      case (foo, incorrectIndex) => foo.attemptTake(candidate(incorrectIndex), incorrectIndex)
    }.misplacedLetterIndices

    val letterFeedbackByIndex =
      (correctIndices.map(_ -> Green) ++ misplacedLetterIndices.map(_ -> Yellow)).toMap.withDefaultValue(Grey)
    WordFeedback(WordIndices.map(letterFeedbackByIndex))
  }

}
