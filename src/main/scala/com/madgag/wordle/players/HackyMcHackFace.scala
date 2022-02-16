package com.madgag.wordle.players

import com.madgag.wordle.approaches.tartan.{Candidates, FeedbackTable}
import com.madgag.wordle.{PlayAnalysis, *}

object HackyMcHackFace {
  def playing(gameMode: GameMode)(using corpus: Corpus): WordlePlayer = {
    val playAnalysis = PlayAnalysis.forGameMode(gameMode)

    case class State(guessIndex: Int, candidates: Candidates) extends WordlePlayer.State {

      lazy val move: Word = {
        (if (candidates.possibleWords.size > 30) {
          println(s"This is supposed to be the easy way - got ${candidates.possibleWords.size} possible words")
          playAnalysis.orderedCandidateOutlooksFor(candidates).head.t // again, a quick hack
        } else {
          // println("Now I'm doing it the hard way")
          playAnalysis.f(guessIndex, candidates).get.wordId
        }).asWord
      }

      def updateWith(evidence: Evidence): State = copy(
        guessIndex = guessIndex + 1,
        candidates = playAnalysis.feedbackTable.update(candidates, evidence)
      )
    }

    new WordlePlayer {
      val start: WordlePlayer.State = State(0, corpus.initialCandidates)
    }
  }

}

