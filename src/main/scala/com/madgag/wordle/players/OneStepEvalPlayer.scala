package com.madgag.wordle.players

import com.madgag.wordle.*
import com.madgag.wordle.approaches.tartan.{Candidates, FeedbackTable}
import com.madgag.wordle.evidence.Evidence

object OneStepEvalPlayer {
  def playing(gameMode: GameMode)(using corpus: Corpus): WordlePlayer = {
    val feedbackTable = FeedbackTable.obtainFor(gameMode)

    case class State(candidates: Candidates) extends WordlePlayer.State {
      lazy val move: Word = feedbackTable.orderedCandidateOutlooksFor(candidates).head.t.asWord

      def updateWith(evidence: Evidence): State = copy(candidates = feedbackTable.update(candidates, evidence))
    }

    new WordlePlayer {
      val start: WordlePlayer.State = State(corpus.initialCandidates)
    }
  }
}