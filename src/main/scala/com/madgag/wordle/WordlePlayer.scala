package com.madgag.wordle

import com.madgag.wordle.evidence.Evidence

trait WordlePlayer {
  def start: WordlePlayer.State
}

object WordlePlayer {
  trait State {
    def move: Word
    
    def updateWith(evidence: Evidence): State
  }
}


