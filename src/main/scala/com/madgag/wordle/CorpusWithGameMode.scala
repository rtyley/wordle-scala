package com.madgag.wordle

import com.madgag.wordle.GameMode.*

import java.nio.file.Path

case class CorpusWithGameMode(corpus: Corpus, gameMode: GameMode) {
  
  val storageDir: Path = corpus.storageDir.resolve(gameMode.getClass.getSimpleName.stripSuffix("$"))
  
  val wordsRequiringEvaluationAsTargets: IndexedSeq[Word] = gameMode.wordsRequiringEvaluationAsTargets(corpus)
}
