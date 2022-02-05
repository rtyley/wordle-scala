package com.madgag.wordle

import com.madgag.wordle.GameMode.*
import com.madgag.wordle.Wordle.Word

import java.nio.file.Path

case class CorpusWithGameMode(corpus: Corpus, gameMode: GameMode) {
  
  val storageDir: Path = Path.of("/tmp", "wordle-scala-cache", corpus.id, gameMode.toString)
  
  val wordsRequiringEvaluationAsTargets: IndexedSeq[Word] = gameMode.wordsRequiringEvaluationAsTargets(corpus)
}
