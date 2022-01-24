package com.madgag.wordle

import org.roaringbitmap.RoaringBitmap

import scala.collection.immutable.BitSet

case class PossibleWords(corpus: Corpus, idsOfPossibleWords: BitSet) {

  val numPossibleWords: Int = idsOfPossibleWords.size

  lazy val possibleWords: Iterable[String] = corpus.humanReadableWordsFor(idsOfPossibleWords)
}

object PossibleWords {
  def allWordsFrom(corpus: Corpus): PossibleWords = PossibleWords(
    corpus,
    corpus.idsForAllCommonWords
  )
  
}
