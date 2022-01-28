package com.madgag.wordle

import scala.collection.immutable.BitSet

case class PossibleWords(corpus: Corpus, wordSet: WordSet) {

  lazy private val idsOfPossibleWords: BitSet = wordSet.bitSet
  lazy val numPossibleWords: Int = idsOfPossibleWords.size

  lazy val possibleWords: Set[String] = ??? // corpus.humanReadableWordsFor(idsOfPossibleWords)
}

object PossibleWords {
  def allWordsFrom(corpus: Corpus): PossibleWords = PossibleWords(
    corpus,
    corpus.idForSetOfAllCommonWords
  )
  
}
