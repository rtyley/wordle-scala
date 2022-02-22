package com.madgag.wordle.evidence

import com.madgag.scala.collection.decorators.*
import com.madgag.wordle.*
import com.madgag.wordle.Wordle.{Letter, WordIndices, WordLength}
import com.madgag.wordle.evidence.LetterFeedback
import com.madgag.wordle.evidence.LetterFeedback.*
import com.madgag.wordle.evidence.WordFeedback.Calculation.BlankCensus
import org.typelevel.literally.Literally

import scala.collection.immutable.Queue
import scala.util.*

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

  def misplacedAndCorrectIndices: (Seq[Int], Seq[Int]) = {
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

  def feedbackFor(guess: Word, actual: Word): WordFeedback =
    guess.zip(actual).foldLeft(BlankCensus)(_.compare.tupled(_)).wordFeedback

  object Calculation {
    
    /**
     * State for 1st pass over Wordle letters, which accomplishes two things:
     *  - Delineates whether guess letters are correct or not
     *  - Counts letters from the actual answer word that ''aren't'' correct, and so will be available to mark
     *    'misplaced' guess-letters in the 2nd pass.
     *
     * @param nonCorrects For each guess-letter: `None` if letter was correct, otherwise populated with the guess-letter
     */
    case class Census(nonCorrects: Queue[Option[Letter]], availableMisplaced: FrequencyMap[Letter]) {
      def compare(guessLetter: Letter, actualLetter: Letter): Census = {
        val notCorrect = guessLetter != actualLetter
        Census(
          nonCorrects = nonCorrects :+ Option.when(notCorrect)(guessLetter),
          availableMisplaced.incrementIf(notCorrect)(actualLetter)
        )
      }

      def wordFeedback: WordFeedback = // perform 2nd pass over Wordle letters
        WordFeedback(nonCorrects.foldLeft(Builder(availableMisplaced))(_ process _).feedback)
    }

    val BlankCensus: Census = Census(Queue.empty, FrequencyMap.empty)

    /**
     * State for 2nd pass over Wordle letters: builds the sequence of letter-feedback, decreasing the count of
     * available misplaced letters as it expends them to mark guess-letters as 'misplaced'.
     */
    private case class Builder(availableMisplaced: FrequencyMap[Letter], feedback: Queue[LetterFeedback] = Queue.empty) {
      /**
       * @param nonCorrectGuess `None` if the guess-letter was correct, otherwise populated with the guess-letter
       */
      def process(nonCorrectGuess: Option[Letter]): Builder = nonCorrectGuess.fold(add(Correct)) { guessLetter =>
        availableMisplaced(guessLetter) match {
          case 0 => add(Incorrect)
          case available => add(Misplaced, availableMisplaced.updated(guessLetter, available - 1))
        }
      }

      private def add(lf: LetterFeedback, updatedMisplaced: FrequencyMap[Letter] = availableMisplaced) =
        copy(feedback = feedback :+ lf, availableMisplaced = updatedMisplaced)
    }
  }
}

extension (inline ctx: StringContext)
  inline def fb(inline args: Any*): WordFeedback = ${WordFeedbackLiteral('ctx, 'args)}

object WordFeedbackLiteral extends Literally[WordFeedback]:
  def validate(s: String)(using Quotes): Either[String, Expr[WordFeedback]] = Try(WordFeedback(s)) match {
    case Success(wordFeedback) => Right('{new WordFeedback(${Expr(wordFeedback.underlying)})})
    case Failure(_) => Left(s"Must be ${Wordle.WordLength} of these emoji: ${LetterFeedback.values.map(_.emoji).mkString}")
  }