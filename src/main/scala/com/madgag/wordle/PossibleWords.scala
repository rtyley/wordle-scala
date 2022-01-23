package com.madgag.wordle

import org.roaringbitmap.RoaringBitmap

case class PossibleWords(corpus: Corpus, idsOfPossibleWords: RoaringBitmap) {

  val numPossibleWords: Int = idsOfPossibleWords.getCardinality

  lazy val possibleWords: Iterable[String] = corpus.humanReadableWordsFor(idsOfPossibleWords)
}

object PossibleWords {
  def allWordsFrom(corpus: Corpus): PossibleWords = PossibleWords(
    corpus,
    corpus.idsForAllCommonWords
  )
  
}
