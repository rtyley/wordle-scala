package com.madgag.wordle

import com.madgag.wordle.Wordle.Word

import java.nio.file.Path
import scala.collection.immutable.ArraySeq
import scala.util.hashing.MurmurHash3

case class Corpus(words: Set[Word]) {
  val numWords: Int = words.size

  val orderedWords: IndexedSeq[Word] = words.toIndexedSeq

  val hash: Int = MurmurHash3.orderedHashing.hash(orderedWords)

  val assayStoragePath: Path = Path.of("/tmp", s"$hash.json")
}
