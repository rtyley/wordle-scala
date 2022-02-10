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

case class FParams(guessIndex: Int, h: Candidates)
case class FResult(beta: Int, wordGuessSum: WordGuessSum)


class GarpGarp(
  analysisForCorpusWithGameMode: AnalysisForCorpusWithGameMode
) {
  case class CandidatesPartition(possibleCandidates: Seq[Candidates]) {

    override val hashCode: Int = possibleCandidates.hashCode() // we rely on the hashcode a lot for `Set`s, so compute once...!

    val evennessScore: Int = possibleCandidates.map(c => c.possibleWords.size * c.possibleWords.size).sum

    // val borg: Seq[AtomicReference]

    def findRequiredGuessesWithPerfectPlay(thresholdToBeat: Int, nextGuessIndex: Int): Option[Int] = {
      possibleCandidates.foldM(0) {
        case (acc, candidates) if thresholdToBeat > acc =>
          Some(acc + f(nextGuessIndex, candidates, thresholdToBeat - acc).guessSum)
        case _ => None
      }.filter(_ < thresholdToBeat)
    }
  }

  case class PossCanSetsIfCanPlayed(t: WordId, candidatesPartition: CandidatesPartition) {
    def findCandidateScoringBetterThan(thresholdToBeat: Int, nextGuessIndex: Int): Option[WordGuessSum] = {
      candidatesPartition.findRequiredGuessesWithPerfectPlay(thresholdToBeat, nextGuessIndex).map {
        newBestScore => WordGuessSum(t, newBestScore)
      }
    }
  }

  val newBestScoreCounter = new LongAdder()
  val callsToFCounter = new LongAdder()

  val fResultsByFParams: java.util.concurrent.ConcurrentMap[FParams,FResult] =
    new java.util.concurrent.ConcurrentHashMap()

  /**
   *
   * @param beta only pursue results that are better (lower) than this threshold - results that >= to this threshold
   *             are useless.
   * @return accurate result, if one could be found below the beta threshold
   */
  def f(guessIndex: Int, h: Candidates, beta: Int = 1000000): WordGuessSum = if (guessIndex>=6) WordGuessSum(-1,1000000) else {
    callsToFCounter.increment()
    val numPossibleWords = h.possibleWords.size
    numPossibleWords match {
      case 0 => throw new IllegalStateException("Can't be!")
      case 1 => WordGuessSum(h.possibleWords.head, 1)
      case 2 => WordGuessSum(h.possibleWords.head, 3)
      case _ => {
        // Hit cache for FParams
        // If miss - calculate the stuff, store it
        // if hit - can we use the calculated data? YES if:
        // OLD-beta > NEW-beta --
        //   ie, we searched really hard, and permitted some really bad answers which you should now feel free to discard,
        //
        // or... old-result < NEW-beta
        // typically old-result (if found) < OLD-beta
        // if OLD-beta < NEW-beta, that doesn't mean that we can't use old-result, so long as it's < NEW-beta
        // if old-result > NEW-beta (a failed-to-find solution), surely we can still use old-result so long as OLD-beta > NEW-beta

        val fParams = FParams(guessIndex, h)
        val nextGuessIndex = guessIndex + 1

        Option(fResultsByFParams.get(fParams)).filter(oldRes => oldRes.beta > beta || oldRes.wordGuessSum.guessSum < beta).getOrElse {
          val newResult: WordGuessSum = h.allWords.toSeq.map { t =>
            possibleCandidateSetsIfCandidatePlayed(h, t)
          }.distinctBy(_.fastCandidatesSetHash).sortBy(_.partitionEvennessScore).foldLeft(WordGuessSum(-1, beta)) {
            case (bestSoFar, possCanSetsIfCanPlayed) =>
              possCanSetsIfCanPlayed.findCandidateScoringBetterThan(bestSoFar.guessSum, nextGuessIndex).getOrElse(bestSoFar)
          }.addGuesses(h.possibleWords.size)

          val rrr = FResult(beta, newResult)
          fResultsByFParams.merge(fParams, rrr, {
            case (_, currentResult) =>
              Option(currentResult).filter(_.beta > beta).getOrElse(rrr)
          })
        }.wordGuessSum
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
        CandidatesPartition(
          (analysisForCorpusWithGameMode.possibleCandidateSetsIfCandidatePlayed(h, t) - WordFeedback.CompleteSuccess).values.toSeq.sortBy(_.possibleWords.size)
        )
      )
    })
  }
}

