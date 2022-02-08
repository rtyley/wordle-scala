package com.madgag.wordle

import com.google.common.util.concurrent.{AtomicDouble, Atomics}
import com.madgag.wordle.approaches.tartan.{AnalysisForCorpusWithGameMode, Candidates}

import java.util.concurrent.atomic.{AtomicInteger, LongAdder}

// import scala.collection.parallel.CollectionConverters.*
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

case class Explorer(
  analysisForCorpusWithGameMode: AnalysisForCorpusWithGameMode,
  successValues: SuccessValues
) {
  val bcCounter = new LongAdder()
  def bestCandidate(guessIndex: Int, candidates: Candidates): WordId = {
    val bestScoreSoFar = new AtomicDouble(0)
    Await.result(Future.traverse(candidates.allWords) { wordId =>
      Future(wordId -> {
        val score = expectedUtility(guessIndex, wordId, candidates, bestScoreSoFar.get().toFloat)
        val latestBestScore = bestScoreSoFar.get()
        if (score > latestBestScore) {
          bcCounter.increment()
          println(s"$latestBestScore -> $score")
          bestScoreSoFar.compareAndSet(latestBestScore,score)
        }
        score
      })
    }, Duration.Inf).maxBy(_._2)._1
    // candidates.allWords.maxBy(candidateWordId => expectedUtility(guessIndex, candidateWordId, candidates))
  }

  val euCounter = new LongAdder()
  def expectedUtility(guessIndex: Int, candidateId: WordId, candidates: Candidates, bestScoreSoFar: Float = 0): Float = {
    val numPossibilitiesOfImmediateSuccessWithGuess =
      if (candidates.possibleWords.contains(candidateId)) 1 else 0

    val nextGuessIndex = guessIndex + 1
    val expectedUtilityOfImmediateSuccess = successValues(guessIndex) * numPossibilitiesOfImmediateSuccessWithGuess
    val numPossibleWords = candidates.possibleWords.size

    (if (nextGuessIndex >= successValues.seq.size) expectedUtilityOfImmediateSuccess else expectedUtilityOfImmediateSuccess + {
      val possibleCandidateSets: Set[Candidates] =
        (analysisForCorpusWithGameMode.possibleCandidateSetsIfCandidatePlayed(candidates, candidateId) - WordFeedback.CompleteSuccess).values.toSet

      val maxPossibleUtilityFromLaterWins = (numPossibleWords - numPossibilitiesOfImmediateSuccessWithGuess) * successValues(nextGuessIndex)
      if (bestScoreSoFar>(expectedUtilityOfImmediateSuccess+maxPossibleUtilityFromLaterWins)/numPossibleWords) {
        euCounter.increment()
        0
      } else if (nextGuessIsCertainSuccessGiven(possibleCandidateSets)) maxPossibleUtilityFromLaterWins else
        possibleCandidateSets.map(utilityOfBestCandidateOn(nextGuessIndex)).sum
    }) / numPossibleWords
  }

  private def utilityOfBestCandidateOn(guessIndex: Int)(candidates: Candidates) = {
    candidates.possibleWords.size * candidates.allWords.foldLeft(0f) {
      case (bestScore, candidateId) =>
        val score = expectedUtility(guessIndex, candidateId, candidates, bestScore)
        Math.max(score, bestScore)
    }
//
//
//    candidates.possibleWords.size *
//      candidates.allWords.map(nextCandidateId => expectedUtility(guessIndex, nextCandidateId, candidates)).max
  }

  private def nextGuessIsCertainSuccessGiven(possibleCandidateSets: Set[Candidates]): Boolean =
    possibleCandidateSets.forall(_.possibleWords.size == 1)
}
