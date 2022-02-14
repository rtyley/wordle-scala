package com.madgag.wordle

trait WordlePlayer {
  def start(gameMode: GameMode)(using Corpus): WordlePlayer.State
}

object WordlePlayer {
  trait State {
    def move: Word

    def updateWith(evidence: Evidence): State
  }
}


