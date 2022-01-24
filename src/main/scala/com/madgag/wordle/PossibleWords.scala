package com.madgag.wordle

import com.madgag.wordle.PossibleWordSetStore.wordSetFor
import org.roaringbitmap.RoaringBitmap

import scala.collection.immutable.BitSet

case class PossibleWords(corpus: Corpus, wordSetId: WordSetId) {

  lazy val idsOfPossibleWords: BitSet = wordSetFor(wordSetId)
  lazy val numPossibleWords: Int = idsOfPossibleWords.size

  lazy val possibleWords: Set[String] = corpus.humanReadableWordsFor(idsOfPossibleWords)
}

object PossibleWords {
  def allWordsFrom(corpus: Corpus): PossibleWords = PossibleWords(
    corpus,
    corpus.idForSetOfAllCommonWords
  )
  
}
