package com.madgag.wordle

import com.google.common.io.{CharStreams, Resources}
import com.madgag.wordle.WordFeedback.feedbackFor
import com.madgag.wordle.Wordle.Word
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

  val gridStorage: File = Path.of("/tmp", "wordle-scala-cache", s"$hash.data.gz").toFile

  val initialCandidates: Candidates = Candidates(
    possibleWords = BitSet.fromSpecific(0 until commonWords.size),
    discriminators = BitSet.fromSpecific(commonWords.size until allWordsOrdered.size)
  )

  def idFor(word: Word): WordId = allWordsOrdered.indexOf(word)

  def pickRandomTargetWord(): Word = commonWordsOrdered(Random.nextInt(commonWordsOrdered.size))

  def updateCandidatesRemovingPossibleWord(candidates: Candidates, wordId: WordId): Candidates = {
    updateCandidatesWithNewPossibleWordSet(candidates, candidates.possibleWords - wordId)
  }
  
  def updateCandidatesWithNewPossibleWordSet(candidates: Candidates, updatedPossibleWords: SortedSet[WordId]): Candidates = {
    updateCandidatesWith(candidates, updatedPossibleWords, candidates.possibleWords -- updatedPossibleWords)
  }

  def updateCandidatesWithEvidence(candidates: Candidates, evidenceWordId: WordId, evidenceFeedback: WordFeedback): Candidates = {
    val (possibleWordsThatRemainPossible, possibleWordsThatBecameImpossible) = {
      val gridEntryForEvidenceWord = grid(evidenceWordId)
      candidates.possibleWords.partition(gridEntryForEvidenceWord(_) == evidenceFeedback.underlying)
    }

    updateCandidatesWith(candidates, possibleWordsThatRemainPossible, possibleWordsThatBecameImpossible)
  }

  private def updateCandidatesWith(candidates: Candidates, possibleWordsThatRemainPossible: SortedSet[WordId], possibleWordsThatBecameImpossible: SortedSet[WordId]) = {
    // if the `Candidates` for possibleWordsThatRemainPossible is already available, return it, skip next part

    val updatedDiscriminators: SortedSet[WordId] = (candidates.discriminators ++ possibleWordsThatBecameImpossible).filter { wordId =>
      val gridEntryForWord = grid(wordId)
      val firstFeedback = gridEntryForWord(possibleWordsThatRemainPossible.head)
      possibleWordsThatRemainPossible.view.tail.exists(gridEntryForWord(_) != firstFeedback)
    }

    Candidates(
      possibleWords = possibleWordsThatRemainPossible,
      discriminators = updatedDiscriminators
    )
  }

  def possibleWordSetsOnCandidate(candidates: Candidates, candidateWordId: WordId): Set[SortedSet[WordId]] = {
    val gridEntryForWord = grid(candidateWordId)
    candidates.possibleWords.groupBy(gridEntryForWord).values.toSet
  }

  def possibleWordSetsForCandidates(candidates: Candidates): Set[Set[SortedSet[WordId]]] = {
    candidates.allWords.toSet.map(wordId => possibleWordSetsOnCandidate(candidates, wordId))
  }

  def possibleCandidatesAfterNextPlayOn(candidates: Candidates): Set[Candidates] = {
    Await.result(Future.traverse(possibleWordSetsForCandidates(candidates).flatten) { possWordset =>
      Future(updateCandidatesWithNewPossibleWordSet(candidates, possWordset))
    }, Duration.Inf)
  }

  lazy val grid: Array[Array[Byte]] = {
    println(gridStorage.getAbsolutePath)
    if (gridStorage.exists()) {
      Using.resource(new ObjectInputStream(new GZIPInputStream(new FileInputStream(gridStorage)))) { s =>
        s.readObject().asInstanceOf[Array[Array[Byte]]]
      }
    } else {
      val tmpFile = File.createTempFile("temp", "grid")
      val gridAsBytes = for {
        candidateWord <- allWordsOrdered.toArray
      } yield {
        for (targetWord <- commonWordsOrdered) yield feedbackFor(candidateWord, targetWord).underlying
      }.toArray

      Using.resource(new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(tmpFile)))) { s =>
        s.writeObject(gridAsBytes)
      }

      gridStorage.getParentFile.mkdirs()
      Files.move(tmpFile.toPath, gridStorage.toPath)
      gridAsBytes
    }
  }



  def analyseGrid() = {
    val strategies = Seq(BitSetSize, BestOfStrategy(Seq(ShortArraySize, InvertedShortArraySize)))

    def sizeHistogramOf(idSets: Set[Set[WordId]], bucketSize: Int = 200, maxSetSize: Int): String = {
      val setQuantityBySize: SortedMap[Int, Int] = SortedMap.from(idSets.groupBy(_.size).mapV(_.size))
      val bucketSizeByBucket: SortedMap[Int, Int] = SortedMap.from(setQuantityBySize.groupBy {
        case (setSize, quantity) => (setSize / bucketSize) * bucketSize
      }.mapV(_.values.sum))

      val histogram = {
        val maxBucketSize = bucketSizeByBucket.values.max
        (for ((bucket, bucketSize) <- bucketSizeByBucket) yield {
          f"$bucket%5d : $bucketSize%6d ${Seq.fill(60 * bucketSize / maxBucketSize)("■").mkString}"
        }).mkString("\n")
      }
      val storageSummary = strategies.map { strategy =>
        f"${strategy.totalSizeFor(setQuantityBySize, maxSetSize)}%12d : $strategy"
      }.mkString("\n")
      s"${idSets.size} sets \n$histogram\nTotal storage required:\n$storageSummary"
    }

    val allPossibleSplitsForCandidates: Set[Set[SortedSet[WordId]]] = possibleWordSetsForCandidates(initialCandidates)
    println(allPossibleSplitsForCandidates.size)

    val possibleCandidatesAfter1stMove: Set[Candidates] = possibleCandidatesAfterNextPlayOn(initialCandidates)
    println(s"...candidates recomputed")

    println(s"possibleCandidatesAfter1stMove=${possibleCandidatesAfter1stMove.size}")

    println(s"possibleWordSetsAfterFirstMove=${sizeHistogramOf(possibleCandidatesAfter1stMove.map(_.possibleWords), 50, 2315)}")
    println(s"possibleDiscriminatorSetsAfterFirstMove=${sizeHistogramOf(possibleCandidatesAfter1stMove.map(_.discriminators), 200,12972)}")

  }

}

object Corpus {
  def fromAsteriskFormat(wordsWithAsterisk: Iterable[String]): Corpus = {
    val (popularWords, unpopularWords) = wordsWithAsterisk.partition(_.endsWith("*"))
    Corpus(SortedSet.from(popularWords.map(_.stripSuffix("*"))), SortedSet.from(unpopularWords))
  }

  def load(): Corpus = Corpus.fromAsteriskFormat(
    Resources.asCharSource(getClass.getResource("/wordle-five-letter-words.txt"), UTF_8).readLines().asScala //.take(4000)
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