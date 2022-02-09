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
  
  // Change to return Option?
  def f(guessIndex: Int, h: Candidates, beta: Int = 1000000): WordGuessSum = if (guessIndex>=6) WordGuessSum(-1,0) else {
    callsToFCounter.increment()
    val numPossibleWords = h.possibleWords.size
    numPossibleWords match {
      case 0 => WordGuessSum(-1,0)
      case 1 => WordGuessSum(h.possibleWords.head,1)
      case 2 => WordGuessSum(h.possibleWords.head,3)
      case _ => {
        val candidatesWithPartitionMostPromisingFirst: Seq[PossCanSetsIfCanPlayed] = h.allWords.toSeq.map { t =>
          possibleCandidateSetsIfCandidatePlayed(h, t)
        }.distinctBy(_.fastCandidatesSetHash).sortBy(_.partitionEvennessScore)

        val nextGuessIndex = guessIndex + 1
        candidatesWithPartitionMostPromisingFirst.foldLeft(WordGuessSum(-1, beta)) { case (bestSoFar, possCanSetsIfCanPlayed) =>
          possCanSetsIfCanPlayed.possibleCandidates.toSeq.foldM(0) {
            case (acc, candidates) if bestSoFar.guessSum > acc => {
              Some(acc + f(nextGuessIndex, candidates, bestSoFar.guessSum - acc).guessSum)
            }
            case _ => None
          }.filter(_ < bestSoFar.guessSum).map {
            newBestScore =>
              if (bestSoFar.wordId != -1) {
                newBestScoreCounter.increment()
              }

              WordGuessSum(possCanSetsIfCanPlayed.t, newBestScore)
          }.getOrElse(bestSoFar)
        }.addGuesses(numPossibleWords)
      }
    }
  }

  val candidateSetsByInput: java.util.concurrent.ConcurrentMap[(WordId, Candidates),PossCanSetsIfCanPlayed] =
    new java.util.concurrent.ConcurrentHashMap()

  val newCandidateSetsRequestedCounter = new LongAdder
  val computeNewCandidateSetsCounter = new LongAdder

  private def possibleCandidateSetsIfCandidatePlayed(h: Candidates, t: WordId): PossCanSetsIfCanPlayed = {
    val key = (t, h)
    newCandidateSetsRequestedCounter.increment()

    candidateSetsByInput.computeIfAbsent(key, { _ =>
      computeNewCandidateSetsCounter.increment()
      PossCanSetsIfCanPlayed(
        t,
        (analysisForCorpusWithGameMode.possibleCandidateSetsIfCandidatePlayed(h, t) - WordFeedback.CompleteSuccess).values.toSeq.sortBy(_.possibleWords.size)
      )
    })
  }
}

case class PossCanSetsIfCanPlayed(t: WordId, possibleCandidates: Seq[Candidates]) {
  val fastCandidatesSetHash: Int = possibleCandidates.hashCode // we rely on the hashcode a lot for `Set`s, so compute once...!

  val partitionEvennessScore: Int = possibleCandidates.map(c => c.possibleWords.size * c.possibleWords.size).sum
}
