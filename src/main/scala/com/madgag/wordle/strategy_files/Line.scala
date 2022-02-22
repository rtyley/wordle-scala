package com.madgag.wordle.strategy_files

import com.madgag.wordle.*
import com.madgag.wordle.Wordle.WordLength
import com.madgag.wordle.evidence.WordFeedback

case class Line(
  guessIndexForHeadFeedbackOrRootWordId: Either[Int,WordId],
  tailPairs: Seq[(WordFeedback, WordId)]
) {
  require(tailPairs.forall(_._1 != WordFeedback.CompleteSuccess))

  val rootWordIdOpt: Option[WordId] = guessIndexForHeadFeedbackOrRootWordId.toOption
  val guessIndexForHeadFeedback: Int = guessIndexForHeadFeedbackOrRootWordId.left.getOrElse(0)

  def text(using Corpus): String = {
    val words: Seq[String] =
      (Seq(rootWordIdOpt) ++ Seq.fill(guessIndexForHeadFeedback)(None) ++ tailPairs.map(p => Some(p._2))).map(_.map(_.asWord).getOrElse(" "*WordLength))
    val feedback: Seq[String] =
      (Seq.fill(guessIndexForHeadFeedback)(None) ++ tailPairs.map(p => Some(p._1)) :+ Some(WordFeedback.CompleteSuccess)).zipWithIndex.map {
        case (feedbackOpt, index) => feedbackOpt.map(_.characters + index).getOrElse(" "*(WordLength+1))
      }
    words.zip(feedback).map(_ + " " + _).mkString(" ")
  }
}

object Line {
  val End: Line = Line(Left(-1), Seq.empty)

  def apply(str: String)(using Corpus): Line = {
    val pairs: IndexedSeq[(Word, String)] = str.grouped((Wordle.WordLength * 2) + 3).map { pairStr =>
        val word = pairStr.substring(0, 5)
        val feedStr = pairStr.substring(6, 11)
        (word, feedStr)
    }.toIndexedSeq

    val rootWordIdOpt: Option[WordId] = {
      val rootWordText = pairs.head._1
      Option.when(!rootWordText.isBlank)(rootWordText.id)
    }
    require(pairs.last._2 == WordFeedback.CompleteSuccess.characters)
    val headGuessIndex = pairs.indexWhere(!_._2.isBlank)
    val pairsWithContent = pairs.drop(headGuessIndex)
    val feedbackWithoutFinalSuccess: Seq[WordFeedback] = pairsWithContent.map(p => WordFeedback.fromChars(p._2))
    val optimalWordIdForFeedback: Seq[WordId] = pairsWithContent.tail.map(_._1.id)

    Line(
      guessIndexForHeadFeedbackOrRootWordId = rootWordIdOpt.toRight(headGuessIndex),
      tailPairs = feedbackWithoutFinalSuccess.zip(optimalWordIdForFeedback)
    )
  }
}