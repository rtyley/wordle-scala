package com.madgag.wordle

import org.roaringbitmap.RoaringBitmap

case class PossibleWords(corpus: Corpus, idsOfPossibleWords: RoaringBitmap) {
  val numPossibleWords: Int = idsOfPossibleWords.getCardinality
}

object PossibleWords {
  def allWordsFrom(corpus: Corpus): PossibleWords = PossibleWords(
    corpus,
    RoaringBitmap.bitmapOfRange(0, corpus.numWords)
  )
  
}
