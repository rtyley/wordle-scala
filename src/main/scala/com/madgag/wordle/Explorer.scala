package com.madgag.wordle

import com.madgag.wordle.approaches.tartan.Candidates

class Explorer(corpus: Corpus, successValues: SuccessValues) {
  def bestCandidate(guessIndex: Int, candidates: Candidates): Int = {
    candidates.allWords.maxBy(candidateWordId => expectedUtility(guessIndex, candidateWordId, candidates))
  }

  def expectedUtility(guessIndex: Int, candidateId: Int, candidates: Candidates): Float = {
    val probSuccessWithThisGuess: Float =
      if (candidates.possibleWords.contains(candidateId)) 1f/candidates.possibleWords.size else 0

    val nextGuessIndex = guessIndex + 1
    val expectedUtilityOfImmediateSuccess = successValues.seq(guessIndex) * probSuccessWithThisGuess

    if (nextGuessIndex >= successValues.seq.size) expectedUtilityOfImmediateSuccess else expectedUtilityOfImmediateSuccess + {
      val candidatesGivenTheGuessIsWrong = corpus.updateCandidatesRemovingPossibleWord(candidates, candidateId)

      (1 - probSuccessWithThisGuess) * expectedUtilityOfLaterGuesses(candidateId, nextGuessIndex, candidatesGivenTheGuessIsWrong)
    }
  }

  @inline private final def expectedUtilityOfLaterGuesses(playedCandidateId: Int, nextGuessIndex: Int, candidates: Candidates) = {
    val possibleCandidateSets: Set[Candidates] =
      corpus.possibleWordSetsOnCandidate(candidates, playedCandidateId).map(pws =>
        corpus.updateCandidatesWithNewPossibleWordSet(candidates, pws)
      )

    val nextGuessIsCertainSuccess: Boolean = possibleCandidateSets.forall(_.possibleWords.size == 1)
    if (nextGuessIsCertainSuccess) successValues(nextGuessIndex) else possibleCandidateSets.map { nextCandidates =>
      val probWeGetThisCandidateSet = nextCandidates.possibleWords.size / candidates.possibleWords.size
      val expectedUtilityOfBestNextCandidate: Float =
        nextCandidates.allWords.map(nextCandidateId => expectedUtility(nextGuessIndex, nextCandidateId, nextCandidates)).max
      expectedUtilityOfBestNextCandidate * probWeGetThisCandidateSet
    }.sum
  }
}
