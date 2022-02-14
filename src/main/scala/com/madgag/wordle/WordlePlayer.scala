package com.madgag.wordle

trait WordlePlayer {
  def start(gameMode: GameMode)(using Corpus): WordlePlayerState
}

trait WordlePlayerState {
  def move: Word

  def updateWith(evidence: Evidence): WordlePlayerState
}
