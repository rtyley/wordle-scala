package com.madgag.wordle.strategy_files

import com.madgag.wordle.*
import com.madgag.wordle.Wordle.WordLength

case class Line(
  headFeedbackGuessIndexOrRootWordId: Either[Int,WordId],
  tailPairs: Seq[(WordFeedback, WordId)]
) {
  require(tailPairs.forall(_._1 != WordFeedback.CompleteSuccess))
}

object Line {
  def apply(str: String)(using Corpus): Line = {
    val rootWordText = str.substring(0,WordLength)
    val rootWordIdOpt: Option[WordId] = Option.when(!rootWordText.isBlank)(rootWordText.id)

    val pairs: IndexedSeq[(String, Word)] = str.drop(WordLength+1).dropRight(WordLength+2).grouped((WordLength * 2) + 3).map {
      pairStr =>
        val feedStr = pairStr.substring(0, WordLength)
        val word = pairStr.substring(WordLength+2, (WordLength*2)+2)
        (feedStr, word)
    }.toIndexedSeq

    val headFeedbackIndex = pairs.indexWhere(!_._1.isBlank)
    require(rootWordIdOpt.isEmpty || headFeedbackIndex==0)

    val feedbackThenOptimalWord: IndexedSeq[(WordFeedback, WordId)] = pairs.drop(headFeedbackIndex).map {
      case (feedbackChars, word) => WordFeedback.fromChars(feedbackChars) -> word.id
    }
    
    Line(
      headFeedbackGuessIndexOrRootWordId = rootWordIdOpt.toRight(headFeedbackIndex),
      tailPairs = feedbackThenOptimalWord
    )
  }
}