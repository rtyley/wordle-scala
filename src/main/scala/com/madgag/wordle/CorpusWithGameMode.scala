package com.madgag.wordle

import com.madgag.wordle.GameMode.*

import java.nio.file.Path

case class CorpusWithGameMode(corpus: Corpus, gameMode: GameMode) {
  
  val storageDir: Path = Path.of("/tmp", "wordle-scala-cache", corpus.id, gameMode.getClass.getSimpleName)
  
  val wordsRequiringEvaluationAsTargets: IndexedSeq[Word] = gameMode.wordsRequiringEvaluationAsTargets(corpus)
}
