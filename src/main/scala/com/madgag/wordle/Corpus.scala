package com.madgag.wordle

import com.google.common.io.{CharStreams, Resources}
import com.madgag.wordle.WordFeedback.feedbackFor
import com.madgag.wordle.approaches.tartan.Candidates

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import scala.jdk.CollectionConverters.*
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.collection.immutable.{ArraySeq, BitSet, SortedMap, SortedSet}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Random, Using}
import scala.util.hashing.MurmurHash3
import concurrent.ExecutionContext.Implicits.global
import com.madgag.scala.collection.decorators.*

case class Corpus(commonWords: SortedSet[Word], uncommonWords: SortedSet[Word]) {
  val commonWordsOrdered: IndexedSeq[Word] = commonWords.toIndexedSeq.sorted
  val allWordsOrdered: IndexedSeq[Word]  = commonWordsOrdered ++ uncommonWords.toIndexedSeq

  val idForSetOfAllCommonWords: WordSet = PossibleWordSetStore.intern((0 until commonWords.size).toSet)

  val hash: Int = MurmurHash3.orderedHashing.hash(allWordsOrdered)

  val id: String = s"corpus-${commonWords.size}-of-${allWordsOrdered.size}-words__${hash.toHexString.toUpperCase}"

  val initialCandidates: Candidates = Candidates(
    possibleWords = BitSet.fromSpecific(0 until commonWords.size),
    discriminators = BitSet.fromSpecific(commonWords.size until allWordsOrdered.size)
  )

  def withGameMode(gameMode: GameMode): CorpusWithGameMode = CorpusWithGameMode(this, gameMode)

  def idFor(word: Word): WordId = allWordsOrdered.indexOf(word)

  def pickRandomTargetWord(): Word = commonWordsOrdered(Random.nextInt(commonWordsOrdered.size))

}

object Corpus {
  def fromAsteriskFormat(wordsWithAsterisk: Iterable[String]): Corpus = {
    val (popularWords, unpopularWords) = wordsWithAsterisk.partition(_.endsWith("*"))
    Corpus(SortedSet.from(popularWords.map(_.stripSuffix("*"))), SortedSet.from(unpopularWords))
  }

  def load(): Corpus = Corpus.fromAsteriskFormat(
    Resources.asCharSource(getClass.getResource("/wordle-five-letter-words.txt"), UTF_8).readLines().asScala
  )
}

case class SetRequirement(setSize: Int, maxQuantity: Int)

trait SizeEstimator {
  def storageBytesNeededFor(setRequirement: SetRequirement): Long

  def totalSizeFor(setQuantityBySize: SortedMap[Int, Int], maxElementsPerSet: Int): Long = {
    setQuantityBySize.map {
      case (setSize, quantity) => quantity * storageBytesNeededFor(SetRequirement(setSize, maxElementsPerSet))
    }.sum
  }
}

object BitSetSize extends SizeEstimator {
  override def storageBytesNeededFor(setRequirement: SetRequirement): Long =
    Math.ceil(setRequirement.maxQuantity.toFloat / 8).toLong
}

object ShortArraySize extends SizeEstimator {
  override def storageBytesNeededFor(setRequirement: SetRequirement): Long = setRequirement.setSize*2
}

object InvertedShortArraySize extends SizeEstimator {
  override def storageBytesNeededFor(setRequirement: SetRequirement): Long =
    ShortArraySize.storageBytesNeededFor(setRequirement.copy(setSize = setRequirement.maxQuantity - setRequirement.setSize))
}

case class BestOfStrategy(strategies: Seq[SizeEstimator]) extends SizeEstimator {
  override def storageBytesNeededFor(setRequirement: SetRequirement): Long = {
    strategies.map(_.storageBytesNeededFor(setRequirement)).min
  }
}