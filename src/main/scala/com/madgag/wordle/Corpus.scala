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
import com.madgag.wordle.wordsets.{ShortArrayWordSet, WordSet}

case class Corpus(commonWords: SortedSet[Word], uncommonWords: SortedSet[Word]) {
  val commonWordsOrdered: IndexedSeq[Word] = commonWords.toIndexedSeq.sorted
  val allWordsOrdered: IndexedSeq[Word]  = commonWordsOrdered ++ uncommonWords.toIndexedSeq

  val hash: Int = MurmurHash3.orderedHashing.hash(allWordsOrdered)

  val id: String = s"corpus-${commonWords.size}-of-${allWordsOrdered.size}-words__${hash.toHexString.toUpperCase}"

  val initialCandidates: Candidates = Candidates(
    possibleWords = WordSet.fromSpecific((0 until commonWords.size).map(_.toShort)),
    discriminators = WordSet.fromSpecific((commonWords.size until allWordsOrdered.size).map(_.toShort))
  )

  val storageDir: Path = Path.of("/tmp", "wordle-scala-cache", id)

  def writeOut(): Unit = {
    storageDir.toFile.mkdirs()
    val hidden = storageDir.resolve("wordlist_hidden")
    val all = storageDir.resolve("wordlist_all")
    Files.write(hidden, commonWords.asJavaCollection)
    Files.write(all, allWordsOrdered.asJavaCollection)
    println(s"Wrote out $id to:\n$hidden\n$all")
  }

  def withGameMode(gameMode: GameMode): CorpusWithGameMode = CorpusWithGameMode(this, gameMode)

  def idFor(word: Word): WordId = allWordsOrdered.indexOf(word).toShort

  def pickRandomTargetWord(): Word = commonWordsOrdered(Random.nextInt(commonWordsOrdered.size))


  def reducedByAFactorOf(factor: Int): Corpus = {
    def winnow(words: SortedSet[Word]):SortedSet[Word] =
      SortedSet.from(words.zipWithIndex.collect {case (e,i) if (i % factor) == 0 => e})
    
    Corpus(winnow(commonWords), winnow(uncommonWords))
  }
}

object Corpus {


  def fromAsteriskFormat(wordsWithAsterisk: String*): Corpus = {
    val (popularWords, unpopularWords) = wordsWithAsterisk.partition(_.endsWith("*"))
    Corpus(SortedSet.from(popularWords.map(_.stripSuffix("*"))), SortedSet.from(unpopularWords))
  }

  def load(): Corpus = Corpus.fromAsteriskFormat(
    Resources.asCharSource(getClass.getResource("/wordle-five-letter-words.txt"), UTF_8).readLines().asScala.toSeq*
  )

  lazy val Full: Corpus = load()
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