package com.madgag.wordle.players

import com.madgag.wordle.approaches.tartan.Candidates
import com.madgag.wordle.*

object HackyMcHackFace extends WordlePlayer {
  def start(gameMode: GameMode)(using corpus: Corpus): WordlePlayer.State = State(
    PlayAnalysis.forGameMode(gameMode),
    0,
    corpus.initialCandidates
  )

  case class State(playAnalysis: PlayAnalysis, guessIndex: Int, candidates: Candidates) extends WordlePlayer.State {
    given corpus: Corpus = playAnalysis.given_Corpus

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
}

