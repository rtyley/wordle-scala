package com.madgag.wordle

import cats.*
import cats.data.*
import cats.implicits.*
import com.madgag.wordle.approaches.tartan.{AnalysisForCorpusWithGameMode, Candidates}

import java.util.concurrent.atomic.LongAdder
import GarpGarp.*

case class WordGuessSum(wordId: WordId, guessSum: Int) extends Ordered[WordGuessSum] {
  override def compare(that: WordGuessSum): Int = guessSum.compareTo(that.guessSum)

  def addGuesses(x: Int) = copy(guessSum = guessSum + x)
}

object GarpGarp {
  def promiseOfCandidateSets(candidateSets: Iterable[Candidates]): Int = {
    candidateSets.map(c => c.possibleWords.size * c.possibleWords.size).sum
  }
}

class GarpGarp(
  analysisForCorpusWithGameMode: AnalysisForCorpusWithGameMode
) {

  val newBestScoreCounter = new LongAdder()
  val callsToFCounter = new LongAdder()


  // Change to return Option?
  def f(guessIndex: Int, h: Candidates, beta: Int = 1000000): WordGuessSum = if (guessIndex>=6) WordGuessSum(-1,0) else {
    callsToFCounter.increment()
    val numPossibleWords = h.possibleWords.size
    numPossibleWords match {
      case 0 => WordGuessSum(-1,0)
      case 1 => WordGuessSum(h.possibleWords.head,1)
      case 2 => WordGuessSum(h.possibleWords.head,3)
      case _ => {
        val candidatesWithPartitionMostPromisingFirst: Seq[(Set[Candidates], WordId)] = h.allWords.toSeq.map { t =>
          possibleCandidateSetsIfCandidatePlayed(h, t) -> t
        }.distinctBy(_._1).sortBy(p => promiseOfCandidateSets(p._1))

        val nextGuessIndex = guessIndex + 1
        val optimised = candidatesWithPartitionMostPromisingFirst.foldLeft(WordGuessSum(-1,beta)) { case (bestSoFar, (possibleCandidateSets, t)) =>
          possibleCandidateSets.toSeq.foldM(0) {
            case (acc, candidates) if bestSoFar.guessSum > acc => {
              Some(acc + f(nextGuessIndex, candidates, bestSoFar.guessSum - acc).guessSum)
            }
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

  val candidateSetsByInput: java.util.concurrent.ConcurrentMap[(WordId, Candidates),Set[Candidates]] =
    new java.util.concurrent.ConcurrentHashMap()

  val newCandidateSetsRequestedCounter = new LongAdder
  val computeNewCandidateSetsCounter = new LongAdder

  private def possibleCandidateSetsIfCandidatePlayed(h: Candidates, t: WordId): Set[Candidates] = {
    val key = (t, h)
    newCandidateSetsRequestedCounter.increment()

    candidateSetsByInput.computeIfAbsent(key, { _ =>
      computeNewCandidateSetsCounter.increment()
      (analysisForCorpusWithGameMode.possibleCandidateSetsIfCandidatePlayed(h, t) - WordFeedback.CompleteSuccess).values.toSet
    })

//    (analysisForCorpusWithGameMode.possibleCandidateSetsIfCandidatePlayed(h, t) - WordFeedback.CompleteSuccess).values.toSet
  }
}

// case class PossCanSetsIfCanPlayed(t: WordId, possibleCandidates: Set[Candidates] )
