package com.madgag.wordle

import com.google.common.io.{CharStreams, Resources}
import com.madgag.wordle.Wordle.Word

import java.nio.charset.StandardCharsets.UTF_8
import scala.jdk.CollectionConverters.*
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path
import scala.collection.immutable.{ArraySeq, BitSet, SortedSet}
import scala.util.hashing.MurmurHash3

case class Corpus(commonWords: SortedSet[Word], uncommonWords: SortedSet[Word]) {
  val commonWordsOrdered: IndexedSeq[Word] = commonWords.toIndexedSeq.sorted
  val allWordsOrdered: IndexedSeq[Word]  = commonWordsOrdered ++ uncommonWords.toIndexedSeq

  val idForSetOfAllCommonWords: WordSet = PossibleWordSetStore.intern((0 until commonWords.size).toSet)

  val hash: Int = MurmurHash3.orderedHashing.hash(allWordsOrdered)

  val assayStoragePath: Path = Path.of("/tmp", "wordle-scala-cache", s"$hash.json")

  // def humanReadableWordsFor(bitMap: BitSet): Set[Word] = bitMap.map(orderedCommonWords(_))
}

object Corpus {
  def fromAsteriskFormat(wordsWithAsterisk: Iterable[String]): Corpus = {
    val (popularWords, unpopularWords) = wordsWithAsterisk.partition(_.endsWith("*"))
    Corpus(popularWords.map(_.stripSuffix("*")).toSet, unpopularWords.toSet)
  }

  def load(): Corpus = Corpus.fromAsteriskFormat(
    Resources.asCharSource(getClass.getResource("/wordle-five-letter-words.txt"), UTF_8).readLines().asScala //.take(4000)
  )
}