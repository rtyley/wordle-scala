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

  def feedbackFor(candidate: Word, actual: Word): WordFeedback = {
    // State used in the fold, _1 is the actual result, _2 is a set of
    // characters already identified as misplaced in the past. If a char was 
    // identified as misplaced, if it appears again a wrong position, it is
    // treated as missing
    type FoldState = (Vector[LetterFeedback], Set[Letter])

    // Makes a map of letters and their frequency
    val baseCharMap = actual.foldLeft(Map[Letter, Int]()) { (m, c) =>
      m.updatedWith(c) {
        _.map(_ + 1).orElse(Some(1))
      }
    }

    // Removes from the map letters that have correct occurrences. That way
    // when we look them up, they're not considered missing, even if they appear
    // in the end of the word.
    val charMap = actual.zip(candidate).filter(_ == _).map(_._1)
      .foldLeft(baseCharMap) { (map, c) =>
        map.updatedWith(c)(_.map(_ - 1))
      }

    // Folds over a zip of both words, assigning status for which one of the
    // pairs
    def updateState(state: FoldState, pair: (Letter, Letter)): FoldState = {
      val (feedback, misplaced) = state
      val (x, y) = pair

      if (x == y) {
        (feedback :+ Correct, misplaced)
      } else if (misplaced.contains(x)) {
        (feedback :+ Incorrect, misplaced)
      } else if (charMap.getOrElse(x, 0) > 0) {
        (feedback :+ Misplaced, misplaced + x)
      } else {
        (feedback :+ Incorrect, misplaced)
      }
    }

    // Does the fold and produces the result
    val result =
      candidate.zip(actual).foldLeft(
        (Vector() -> Set()) : FoldState
      )(updateState)._1

    WordFeedback(result)
  }
}
