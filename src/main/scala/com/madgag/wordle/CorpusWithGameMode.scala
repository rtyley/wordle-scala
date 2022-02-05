package com.madgag.wordle

import java.nio.file.Path

case class CorpusWithGameMode(corpus: Corpus, gameMode: GameMode) {
  
  val storageDir: Path = Path.of("/tmp", "wordle-scala-cache", corpus.id, gameMode.toString)
}
