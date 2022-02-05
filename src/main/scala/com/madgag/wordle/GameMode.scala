package com.madgag.wordle

import com.madgag.wordle.Wordle.Word
import com.madgag.wordle.approaches.tartan.{AnalysisForCorpusWithGameMode, Candidates}

import scala.collection.immutable.SortedSet

sealed trait GameMode {
  def wordsRequiringEvaluationAsTargets(corpus: Corpus): IndexedSeq[Word]

  def possibleCandidateSetsAfter(
    candidates: Candidates,
    playedCandidateId: WordId,
    analysisForCorpusWithGameMode: AnalysisForCorpusWithGameMode
  ): Set[Candidates]
}

object GameMode {
  
  object Normal extends GameMode {
    override def wordsRequiringEvaluationAsTargets(corpus: Corpus): IndexedSeq[Word] = corpus.commonWordsOrdered
    
    /**
     * NORMAL: Partition possible words to comply with feedback, those that do not comply are possible
     * discriminators. Filter those and other discriminators to ensure they still discriminate!
     */
    def possibleCandidateSetsAfter(
      candidates: Candidates,
      playedCandidateId: WordId,
      analysisForCorpusWithGameMode: AnalysisForCorpusWithGameMode
    ): Set[Candidates] = {
      analysisForCorpusWithGameMode.possibleWordSetsOnCandidate(candidates, playedCandidateId).map(pws =>
        analysisForCorpusWithGameMode.updateCandidatesWithNewPossibleWordSet(candidates, pws)
      )
    }
  }

  object Hard extends GameMode {
    def wordsRequiringEvaluationAsTargets(corpus: Corpus) = corpus.allWordsOrdered
    
    /**
     * HARD: Trim both possible words & discriminators to comply with feedback
     */
    def possibleCandidateSetsAfter(
      candidates: Candidates,
      playedCandidateId: WordId,
      analysisForCorpusWithGameMode: AnalysisForCorpusWithGameMode
    ): Set[Candidates] = {
      /**
       * Get possible word sets given played Candidate
       * For each possible word set, there is feedback that would have led to it
       * *ALL* remaining playable words - possible-words & discriminators - must comply with that feedback
       * 
       */

      analysisForCorpusWithGameMode.updateCandidatesGivenHardModePlay(candidates, playedCandidateId)
    }
  }
}