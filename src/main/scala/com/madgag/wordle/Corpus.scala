package com.madgag.wordle

import com.google.common.io.{CharStreams, Resources}
import com.madgag.wordle.Wordle.Word
import org.roaringbitmap.RoaringBitmap

import java.nio.charset.StandardCharsets.UTF_8
import scala.jdk.CollectionConverters.*
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path
import scala.collection.immutable.{ArraySeq, BitSet}
import scala.util.hashing.MurmurHash3

case class Corpus(commonWords: Set[Word], uncommonWords: Set[Word]) {
  val allWordsEvenTheUncommonOnes: Set[Word] = commonWords ++ uncommonWords

  val orderedCommonWords: IndexedSeq[Word] = commonWords.toIndexedSeq.sorted
  val idsForAllCommonWords: BitSet = BitSet.fromSpecific(0 until commonWords.size) // RoaringBitmap.bitmapOfRange(0, commonWords.size)

  val hash: Int = MurmurHash3.orderedHashing.hash(allWordsEvenTheUncommonOnes.toSeq.sorted)

  val assayStoragePath: Path = Path.of("/tmp", s"$hash.json")

  def humanReadable(bitMap: BitSet): String = humanReadableWordsFor(bitMap).mkString(",")

  def humanReadableWordsFor(bitMap: BitSet): Iterable[Word] = bitMap.map(orderedCommonWords(_))
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