package com.madgag.wordle

import cats._
import cats.data._
import com.madgag.wordle.approaches.tartan.{AnalysisForCorpusWithGameMode, Candidates}

case class WordGuessSum(wordId: WordId, guessSum: Int) extends Ordered[WordGuessSum] {
  override def compare(that: WordGuessSum): Int = guessSum.compareTo(that.guessSum)

  def addGuesses(x: Int) = copy(guessSum = guessSum + x)
}

class GarpGarp(
  analysisForCorpusWithGameMode: AnalysisForCorpusWithGameMode
) {

  def f(guessIndex: Int, h: Candidates): WordGuessSum = if (guessIndex>=6) WordGuessSum(0,0) else {
    val numPossibleWords = h.possibleWords.size
    numPossibleWords match {
      case 0 => WordGuessSum(0,0)
      case 1 => WordGuessSum(h.possibleWords.head,1)
      case _ => {
        val candidatesWithPartitionMostPromisingFirst: Seq[(WordId, Seq[Candidates])] = h.allWords.toSeq.map { t =>
          t -> (analysisForCorpusWithGameMode.possibleCandidateSetsIfCandidatePlayed(h, t) - WordFeedback.CompleteSuccess).values.toSeq
        }.sortBy(_._2.map(c => c.possibleWords.size * c.possibleWords.size).sum)

        candidatesWithPartitionMostPromisingFirst.foldLeft(1000000) { case (bestScoreSoFar, (t, possibleCandidates)) =>
          possibleCandidates.foldM(0) {

          }

        }

        candidatesWithPartitionMostPromisingFirst.map { case (t, possibleCandidates) =>
          WordGuessSum(t, possibleCandidates.map(f(guessIndex, _).guessSum).sum)
        }.min.addGuesses(numPossibleWords)
      }
    }
  }
}
