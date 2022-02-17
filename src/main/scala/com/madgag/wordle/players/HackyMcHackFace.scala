package com.madgag.wordle.players

import com.madgag.wordle.approaches.tartan.{Candidates, FeedbackTable}
import com.madgag.wordle.*

object HackyMcHackFace {
  def playing(gameMode: GameMode)(using corpus: Corpus): WordlePlayer = {
    val playAnalysis = new PlayAnalysisForNormalMode()

    case class State(guessIndex: Int, candidates: Candidates) extends WordlePlayer.State {

      lazy val move: Word = {
        (if (candidates.possibleWords.size > 30) {
          println(s"This is supposed to be the easy way - got ${candidates.possibleWords.size} possible words")
          playAnalysis.feedbackTable.orderedCandidateOutlooksFor(candidates).head.t // a quick hack
        } else {
          // println("Now I'm doing it the hard way")
          playAnalysis.f(NormalFPar(guessIndex, candidates.possibleWords)).get.wordId
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

