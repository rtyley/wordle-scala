package com.madgag.wordle

import com.madgag.wordle.Wordle.Word

import scala.collection.immutable.ArraySeq

case class Corpus(words: Set[Word]) {
  val numWords = words.size

  val orderedWords: IndexedSeq[Word] = words.toIndexedSeq
}
