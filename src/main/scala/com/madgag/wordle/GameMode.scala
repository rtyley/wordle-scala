package com.madgag.wordle

import com.madgag.wordle.Wordle.Word
import com.madgag.wordle.approaches.tartan.Candidates

/**
 * NORMAL: Partition possible words to comply with feedback, those that do not comply are possible
 * discriminators. Filter those and other discriminators to ensure they still discriminate!
 *
 * HARD: Trim both possible words & discriminators to comply with feedback
 */
enum GameMode(
  val wordsRequiringEvaluationAsTargets: Corpus => IndexedSeq[Word],
) {
  def possibleCandidateSetsAfter(candidates: Candidates, playedCandidateId: WordId): Set[Candidates]

  case Normal extends GameMode(
    wordsRequiringEvaluationAsTargets = _.commonWordsOrdered
  ) {
    def possibleCandidateSetsAfter(candidates: Candidates, playedCandidateId: WordId): Set[Candidates] = ???
    
  }
  case Hard extends GameMode(
    wordsRequiringEvaluationAsTargets = _.allWordsOrdered
  )
}