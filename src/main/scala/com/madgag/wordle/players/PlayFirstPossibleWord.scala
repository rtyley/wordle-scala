package com.madgag.wordle.players

import com.madgag.wordle.approaches.tartan.{Candidates, FeedbackTable}
import com.madgag.wordle.evidence.Evidence
import com.madgag.wordle.evidence.Evidence.*
import com.madgag.wordle.*

object PlayFirstPossibleWord {
  def player(using corpus: Corpus): WordlePlayer = new WordlePlayer {
    val start: State = State(corpus.commonWords)
  }

  case class State(possibleWords: Iterable[Word]) extends WordlePlayer.State {
    val move: Word = possibleWords.head

    def updateWith(evidence: Evidence): State = State(possibleWords.filter(_.compliesWith(evidence)))
  }
}
