package com.madgag.wordle

import com.madgag.wordle.approaches.tartan.{AnalysisForCorpusWithGameMode, Candidates}

class Explorer(
  analysisForCorpusWithGameMode: AnalysisForCorpusWithGameMode,
  successValues: SuccessValues
) {
  def bestCandidate(guessIndex: Int, candidates: Candidates): Int = {
    candidates.allWords.maxBy(candidateWordId => expectedUtility(guessIndex, candidateWordId, candidates))
  }

  def expectedUtility(guessIndex: Int, candidateId: Int, candidates: Candidates): Float = {
    val numPossibilitiesOfImmediateSuccessWithGuess =
      if (candidates.possibleWords.contains(candidateId)) 1 else 0

    val nextGuessIndex = guessIndex + 1
    val expectedUtilityOfImmediateSuccess = successValues.seq(guessIndex) * numPossibilitiesOfImmediateSuccessWithGuess
    val numPossibleWords = candidates.possibleWords.size

    (if (nextGuessIndex >= successValues.seq.size) expectedUtilityOfImmediateSuccess else expectedUtilityOfImmediateSuccess + {
      val possibleCandidateSets: Set[Candidates] =
        (analysisForCorpusWithGameMode.possibleCandidateSetsIfCandidatePlayed(candidates, candidateId) - WordFeedback.CompleteSuccess).values.toSet

      if (nextGuessIsCertainSuccessGiven(possibleCandidateSets)) (numPossibleWords-numPossibilitiesOfImmediateSuccessWithGuess)*successValues(nextGuessIndex) else possibleCandidateSets.map { nextCandidates =>
        nextCandidates.possibleWords.size *
          nextCandidates.allWords.map(nextCandidateId => expectedUtility(nextGuessIndex, nextCandidateId, nextCandidates)).max
      }.sum
    }) / numPossibleWords
  }

  private def nextGuessIsCertainSuccessGiven(possibleCandidateSets: Set[Candidates]): Boolean =
    possibleCandidateSets.forall(_.possibleWords.size == 1)
}
