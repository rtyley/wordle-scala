package com.madgag.wordle

import com.madgag.scala.collection.decorators.*
import com.madgag.wordle.LetterFeedback.*
import com.madgag.wordle.Wordle.{Letter, WordIndices, WordLength}

class WordFeedback(val underlying: Byte) extends AnyVal {
  def toSeq: Seq[LetterFeedback] = {
    var u: Int = underlying & 0xff
    (for (_ <- WordIndices) yield {
      val letterFeedback = LetterFeedback.fromOrdinal(u % WordFeedback.numValues)
      u /= WordFeedback.numValues
      letterFeedback
    }).reverse
  }
  def emojis: String = toSeq.map(_.emoji).mkString

  def isSuccess: Boolean = toSeq.forall(_ == Correct)

  override def toString: Word = emojis
}

object WordFeedback {
  val numValues: Int = LetterFeedback.values.length
  val CompleteSuccess: WordFeedback = WordFeedback(Seq.fill(WordLength)(Correct))

  def apply(emojiString: String): WordFeedback = {
    def feedbackOnString(str: String): List[LetterFeedback] = if (str.isEmpty) Nil else {
      val lf = LetterFeedback.atStartOfString(str)
      lf :: feedbackOnString(str.substring(lf.emoji.length))
    }

    apply(feedbackOnString(emojiString))
  }

  def apply(l1: LetterFeedback, l2: LetterFeedback, l3: LetterFeedback, l4: LetterFeedback, l5: LetterFeedback): WordFeedback =
    WordFeedback(Seq(l1, l2, l3, l4, l5))

  def apply(letterFeedbacks: Seq[LetterFeedback]): WordFeedback = {
    require(letterFeedbacks.size == WordLength)
    new WordFeedback(
      letterFeedbacks.foldLeft(0) {
        case (total, letterFeedback) => (total * numValues) + letterFeedback.ordinal
      }.toByte
    )
  }

  def feedbackFor(candidate: Word, actual: Word): WordFeedback = {
    val (correctIndices, incorrectIndices) = WordIndices.partition(index => candidate(index) == actual(index))
    val misplacedLetterIndices = incorrectIndices.foldLeft(AvailableAndMisplacedLetters(
      remainingActualLetters = incorrectIndices.map(actual).groupUp(identity)(_.size))
    ) {
      case (availableAndMisplacedLetters, incorrectIndex) => availableAndMisplacedLetters.attemptTake(candidate(incorrectIndex), incorrectIndex)
    }.misplacedLetterIndices

    val letterFeedbackByIndex =
      (correctIndices.map(_ -> Correct) ++ misplacedLetterIndices.map(_ -> Misplaced)).toMap.withDefaultValue(Incorrect)
    WordFeedback(WordIndices.map(letterFeedbackByIndex))
  }

  case class AvailableAndMisplacedLetters(remainingActualLetters: Map[Letter, Int], misplacedLetterIndices: Set[Int] = Set.empty) {
    def attemptTake(letter: Letter, letterIndex: Int): AvailableAndMisplacedLetters = {
      val quantityOfLetterAvailable = remainingActualLetters.getOrElse(letter, 0)
      if (quantityOfLetterAvailable <= 0) this else AvailableAndMisplacedLetters(
        remainingActualLetters.updated(letter, quantityOfLetterAvailable - 1),
        misplacedLetterIndices + letterIndex
      )
    }
  }
}
