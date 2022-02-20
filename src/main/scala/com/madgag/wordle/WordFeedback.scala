package com.madgag.wordle

import com.madgag.scala.collection.decorators.*
import com.madgag.wordle.LetterFeedback.*
import com.madgag.wordle.Wordle.{Letter, WordIndices, WordLength}

import scala.collection.immutable.Queue

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
  def characters: String = toSeq.map(_.character).mkString

  def isSuccess: Boolean = toSeq.forall(_ == Correct)

  def misplacedAndCorrectIndicies: (Seq[Int], Seq[Int]) = {
    val s = toSeq
    (WordIndices.filter(s(_) == Misplaced), WordIndices.filter(s(_) == Correct))
  }

  override def toString: Word = emojis
}

object WordFeedback {

  /**
   * This is the ordering used in the strategy tree files output by
   * https://github.com/alex1770/wordle, eg:
   * `wordle -p tree.txt`
   */
  val AlphabeticalOrdering: Ordering[WordFeedback] = Ordering.by(_.characters)

  val numValues: Int = LetterFeedback.values.length
  val CompleteSuccess: WordFeedback = WordFeedback(Seq.fill(WordLength)(Correct))

  def fromChars(str: String): WordFeedback = {
    require(str.length==WordLength)
    apply(str.map(LetterFeedback.fromCharacter))
  }
  
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

  case class FeedbackBuilder(feedback: Queue[LetterFeedback] = Queue.empty, availableMisplaced: FrequencyMap[Letter]) {
    def compare(guessLetter: Letter, correctLetter: Letter): FeedbackBuilder =
      if (guessLetter == correctLetter) add(Correct) else availableMisplaced(guessLetter) match {
        case 0 => add(Incorrect)
        case available => add(Misplaced, availableMisplaced.updated(guessLetter, available - 1))
      }

    private def add(
      letterFeedback: LetterFeedback,
      updatedAvailableMisplaced: FrequencyMap[Letter] = availableMisplaced
    ) = copy(feedback = feedback :+ letterFeedback, availableMisplaced = updatedAvailableMisplaced)
  }

  def feedbackFor(candidate: Word, actual: Word): WordFeedback = {
    val guessWithActual = candidate.zip(actual)

    WordFeedback(guessWithActual.foldLeft(FeedbackBuilder(
      availableMisplaced = guessWithActual.foldLeft(FrequencyMap.empty) {
        case (freqMap, (guessLetter, actualLetter)) => freqMap.incrementIf(guessLetter != actualLetter)(actualLetter)
    }))(_.compare.tupled(_)).feedback)
  }
}
