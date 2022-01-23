package com.madgag.wordle

import com.google.common.io.{CharStreams, Resources}
import com.madgag.wordle.Wordle.Word
import org.roaringbitmap.RoaringBitmap

import java.nio.charset.StandardCharsets.UTF_8
import scala.jdk.CollectionConverters.*
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path
import scala.collection.immutable.ArraySeq
import scala.util.hashing.MurmurHash3

case class Corpus(popularWords: Set[Word], unpopularWords: Set[Word]) {
  val words: Set[Word] = popularWords ++ unpopularWords

  val numWords: Int = words.size

  val orderedWords: IndexedSeq[Word] = words.toIndexedSeq

  val hash: Int = MurmurHash3.orderedHashing.hash(orderedWords)

  val assayStoragePath: Path = Path.of("/tmp", s"$hash.json")

  def humanReadable(bitMap: RoaringBitmap): String = bitMap.asScala.map(orderedWords(_)).mkString(",")
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