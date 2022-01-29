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
import scala.util.Using
import scala.util.hashing.MurmurHash3
import concurrent.ExecutionContext.Implicits.global
import com.madgag.scala.collection.decorators.*

case class Corpus(commonWords: SortedSet[Word], uncommonWords: SortedSet[Word]) {
  val commonWordsOrdered: IndexedSeq[Word] = commonWords.toIndexedSeq.sorted
  val allWordsOrdered: IndexedSeq[Word]  = commonWordsOrdered ++ uncommonWords.toIndexedSeq

  val idForSetOfAllCommonWords: WordSet = PossibleWordSetStore.intern((0 until commonWords.size).toSet)

  val hash: Int = MurmurHash3.orderedHashing.hash(allWordsOrdered)

  val gridStorage: File = Path.of("/tmp", "wordle-scala-cache", s"$hash.data.gz").toFile

  // def humanReadableWordsFor(bitMap: BitSet): Set[Word] = bitMap.map(orderedCommonWords(_))

  val initialCandidates: Candidates = Candidates(
    possibleWords = BitSet.fromSpecific(0 until commonWords.size),
    discriminators = BitSet.fromSpecific(commonWords.size until allWordsOrdered.size)
  )

  def updateCandidatesWithNewPossibleWordSet(candidates: Candidates, updatedPossibleWords: SortedSet[Int]): Candidates = {
    updateCandidatesWith(candidates, updatedPossibleWords, candidates.possibleWords -- updatedPossibleWords)
  }

  def updateCandidatesWithEvidence(candidates: Candidates, evidenceWordId: Int, evidenceFeedback: WordFeedback): Candidates = {
    val (possibleWordsThatRemainPossible, possibleWordsThatBecameImpossible) = {
      val gridEntryForEvidenceWord = grid(evidenceWordId)
      candidates.possibleWords.partition(gridEntryForEvidenceWord(_) == evidenceFeedback.underlying)
    }

    updateCandidatesWith(candidates, possibleWordsThatRemainPossible, possibleWordsThatBecameImpossible)
  }

  private def updateCandidatesWith(candidates: Candidates, possibleWordsThatRemainPossible: SortedSet[Int], possibleWordsThatBecameImpossible: SortedSet[Int]) = {
    // if the `Candidates` for possibleWordsThatRemainPossible is already available, return it, skip next part

    val updatedDiscriminators: SortedSet[Int] = (candidates.discriminators ++ possibleWordsThatBecameImpossible).filter { wordId =>
      val gridEntryForWord = grid(wordId)
      val firstFeedback = gridEntryForWord(possibleWordsThatRemainPossible.head)
      possibleWordsThatRemainPossible.view.tail.exists(gridEntryForWord(_) != firstFeedback)
    }

    Candidates(
      possibleWords = possibleWordsThatRemainPossible,
      discriminators = updatedDiscriminators
    )
  }

  def possibleWordSetsOnCandidate(candidates: Candidates, candidateWordId: Int): Set[SortedSet[Int]] = {
    val gridEntryForWord = grid(candidateWordId)
    candidates.possibleWords.groupBy(gridEntryForWord).values.toSet
  }

  def possibleWordSetsForCandidates(candidates: Candidates): Set[Set[SortedSet[Int]]] = {
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
      val tmpFile = File.createTempFile("temp", "skunky")
      val gd = for {
        candidateWord <- allWordsOrdered.toArray
      } yield {
        println(s"candidateWord=$candidateWord")
        for (targetWord <- commonWordsOrdered) yield feedbackFor(candidateWord, targetWord).underlying
      }.toArray

      Using.resource(new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(tmpFile)))) { s =>
        s.writeObject(gd)
      }

      gridStorage.getParentFile.mkdirs()
      Files.move(tmpFile.toPath, gridStorage.toPath)
      gd
    }
  }

  def analyseGrid() = {
    def sizeHistogramOf(idSets: Set[Set[Int]], bucketSize: Int = 200): String = {
      s"${idSets.size} sets \n" + {
        val bucketSizeByBucket: SortedMap[Int, Int] = SortedMap.from(idSets.groupBy(idSet => (idSet.size / bucketSize) * bucketSize).mapV(_.size))
        val maxBucketSize = bucketSizeByBucket.values.max
        (for ((bucket, bucketSize) <- bucketSizeByBucket) yield {
          f"$bucket%5d : $bucketSize%6d ${Seq.fill(60 * bucketSize / maxBucketSize)("■").mkString}"
        }).mkString("\n")
      }
    }

    val allPossibleSplitsForCandidates: Set[Set[SortedSet[Int]]] = possibleWordSetsForCandidates(initialCandidates)
    println(allPossibleSplitsForCandidates.size)

    val possibleCandidatesAfter1stMove: Set[Candidates] = possibleCandidatesAfterNextPlayOn(initialCandidates)
    println(s"...candidates recomputed")

    println(s"possibleCandidatesAfter1stMove=${possibleCandidatesAfter1stMove.size}")

    println(s"possibleWordSetsAfterFirstMove=${sizeHistogramOf(possibleCandidatesAfter1stMove.map(_.possibleWords), 50)}")
    println(s"possibleDiscriminatorSetsAfterFirstMove=${sizeHistogramOf(possibleCandidatesAfter1stMove.map(_.discriminators))}")

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