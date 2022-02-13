package com.madgag.wordle.strategy_files

import com.madgag.wordle.*

case class Line(
  headGuessIndex: Int,
  headWordId: Option[WordId],
  headFeedback: WordFeedback,
  tailPairs: Seq[(WordId, WordFeedback)]
) {
  require(tailPairs.last._2 == WordFeedback.CompleteSuccess)
}

object Line {
  def apply(str: String)(using Corpus): Line = {
    val pairs: IndexedSeq[(String, String)] = str.grouped((Wordle.WordLength * 2) + 3).map {
      pairStr =>
        val word = pairStr.substring(0, 5)
        val feedStr = pairStr.substring(6, 11)
        (word, feedStr)
    }.toIndexedSeq

    val headGuessIndex = pairs.indexWhere(!_._2.isBlank)
    val headPair = pairs(headGuessIndex)
    
    Line(
      headGuessIndex = headGuessIndex,
      headWordId = Option.when(!headPair._1.isBlank)(headPair._1.id),
      headFeedback = WordFeedback.fromChars(headPair._2),
      tailPairs = pairs.drop(headGuessIndex+1).map {
        case (word, feedStr) => (word.id, WordFeedback.fromChars(feedStr))
      }
    )
  }
}