package com.madgag.wordle

import com.madgag.wordle.approaches.tartan.{AnalysisForCorpusWithGameMode, Candidates}

// import scala.collection.parallel.CollectionConverters.*
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

case class Explorer(
  analysisForCorpusWithGameMode: AnalysisForCorpusWithGameMode,
  successValues: SuccessValues
) {
  def bestCandidate(guessIndex: Int, candidates: Candidates): WordId = {
    Await.result(Future.traverse(candidates.allWords) { wordId =>
      Future(wordId -> expectedUtility(guessIndex, wordId, candidates))
    }, Duration.Inf).maxBy(_._2)._1
    // candidates.allWords.maxBy(candidateWordId => expectedUtility(guessIndex, candidateWordId, candidates))
  }

  def expectedUtility(guessIndex: Int, candidateId: WordId, candidates: Candidates): Float = {
    val numPossibilitiesOfImmediateSuccessWithGuess =
      if (candidates.possibleWords.contains(candidateId)) 1 else 0

    val nextGuessIndex = guessIndex + 1
    val expectedUtilityOfImmediateSuccess = successValues(guessIndex) * numPossibilitiesOfImmediateSuccessWithGuess
    val numPossibleWords = candidates.possibleWords.size

    (if (nextGuessIndex >= successValues.seq.size) expectedUtilityOfImmediateSuccess else expectedUtilityOfImmediateSuccess + {
      val possibleCandidateSets: Set[Candidates] =
        (analysisForCorpusWithGameMode.possibleCandidateSetsIfCandidatePlayed(candidates, candidateId) - WordFeedback.CompleteSuccess).values.toSet

      if (nextGuessIsCertainSuccessGiven(possibleCandidateSets)) {
        (numPossibleWords-numPossibilitiesOfImmediateSuccessWithGuess)*successValues(nextGuessIndex)
      } else possibleCandidateSets.map(utilityOfBestCandidateOn(nextGuessIndex)).sum
    }) / numPossibleWords
  }

  private def utilityOfBestCandidateOn(guessIndex: Int)(candidates: Candidates) = {
    candidates.possibleWords.size *
      candidates.allWords.map(nextCandidateId => expectedUtility(guessIndex, nextCandidateId, candidates)).max
  }

  private def nextGuessIsCertainSuccessGiven(possibleCandidateSets: Set[Candidates]): Boolean =
    possibleCandidateSets.forall(_.possibleWords.size == 1)
}
