package com.madgag.wordle

import cats.*
import cats.data.*
import cats.implicits.*
import com.madgag.wordle.approaches.tartan.{AnalysisForCorpusWithGameMode, Candidates}

import java.util.concurrent.atomic.LongAdder

case class WordGuessSum(wordId: WordId, guessSum: Int) extends Ordered[WordGuessSum] {
  override def compare(that: WordGuessSum): Int = guessSum.compareTo(that.guessSum)

  def addGuesses(x: Int) = copy(guessSum = guessSum + x)
}

class GarpGarp(
  analysisForCorpusWithGameMode: AnalysisForCorpusWithGameMode
) {

  val newBestScoreCounter = new LongAdder()
  val callsToFCounter = new LongAdder()

  def f(guessIndex: Int, h: Candidates): WordGuessSum = if (guessIndex>=6) WordGuessSum(-1,0) else {
    callsToFCounter.increment()
    val numPossibleWords = h.possibleWords.size
    numPossibleWords match {
      case 0 => WordGuessSum(-1,0)
      case 1 => WordGuessSum(h.possibleWords.head,1)
      case _ => {
        val candidatesWithPartitionMostPromisingFirst: Seq[(WordId, Seq[Candidates])] = h.allWords.toSeq.map { t =>
          t -> (analysisForCorpusWithGameMode.possibleCandidateSetsIfCandidatePlayed(h, t) - WordFeedback.CompleteSuccess).values.toSeq
        }.sortBy(_._2.map(c => c.possibleWords.size * c.possibleWords.size).sum)

        val nextGuessIndex = guessIndex + 1
        val optimised = candidatesWithPartitionMostPromisingFirst.foldLeft(WordGuessSum(-1,1000000)) { case (bestSoFar, (t, possibleCandidateSets)) =>
          possibleCandidateSets.foldM(0) {
            case (acc, candidates) if bestSoFar.guessSum > acc => Some(acc + f(nextGuessIndex, candidates).guessSum)
            case _ => None
          }.filter(_ < bestSoFar.guessSum).map {
            newBestScore =>
              if (bestSoFar.wordId != -1) {
                newBestScoreCounter.increment()
              }

              WordGuessSum(t, newBestScore)
          }.getOrElse(bestSoFar)
        }.addGuesses(numPossibleWords)

//        val naive = candidatesWithPartitionMostPromisingFirst.map { case (t, possibleCandidates) =>
//          WordGuessSum(t, possibleCandidates.map(f(nextGuessIndex, _).guessSum).sum)
//        }.min.addGuesses(numPossibleWords)
//        require(optimised.guessSum==naive.guessSum)

        optimised
      }
    }
  }
}
