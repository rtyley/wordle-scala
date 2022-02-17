package com.madgag.wordle.players

import com.madgag.wordle.*
import com.madgag.wordle.Evidence.*
import com.madgag.wordle.{Corpus, Evidence, GameMode, WordlePlayer}
import com.madgag.wordle.approaches.tartan.{Candidates, FeedbackTable}

object PlayFirstPossibleWord {
  def player(using corpus: Corpus): WordlePlayer = new WordlePlayer {
    val start: State = State(corpus.commonWords)
  }

  case class State(possibleWords: Iterable[Word]) extends WordlePlayer.State {
    val move: Word = possibleWords.head

    def updateWith(evidence: Evidence): State = State(possibleWords.filter(_.compliesWith(evidence)))
  }
}
