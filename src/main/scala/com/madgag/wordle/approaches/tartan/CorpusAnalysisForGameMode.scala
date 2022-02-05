package com.madgag.wordle.approaches.tartan

import com.madgag.wordle.*
import com.madgag.wordle.WordFeedback.feedbackFor
import com.madgag.wordle.{BestOfStrategy, BitSetSize, Corpus, CorpusWithGameMode, GameMode, InvertedShortArraySize, LocalFileCache, ShortArraySize, WordFeedback}

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.file.Files
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Using
import com.madgag.scala.collection.decorators.*

import concurrent.ExecutionContext.Implicits.global

class AnalysisForCorpusWithGameMode(corpusWithGameMode: CorpusWithGameMode, grid: Array[Array[Byte]]) {
  val corpus: Corpus = corpusWithGameMode.corpus

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

  def possibleCandidateSetsAfter(candidates: Candidates, playedCandidateId: WordId): Set[Candidates] = {
    /**
     * NORMAL: Partition possible words to comply with feedback, those that do not comply are possible
     * discriminators. Filter those and other discriminators to ensure they still discriminate!
     *
     * HARD: Trim both possible words & discriminators to comply with feedback
     */

    possibleWordSetsOnCandidate(candidates, playedCandidateId).map(pws =>
      updateCandidatesWithNewPossibleWordSet(candidates, pws)
    )
  }

  def possibleWordSetsOnCandidate(candidates: Candidates, candidateWordId: WordId): Set[SortedSet[WordId]] = {
    val gridEntryForWord = grid(candidateWordId)
    candidates.possibleWords.groupBy(gridEntryForWord).values.toSet
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

//    val allPossibleSplitsForCandidates: Set[Set[SortedSet[WordId]]] = possibleWordSetsForCandidates(corpus.initialCandidates)
//    println(allPossibleSplitsForCandidates.size)

//    val possibleCandidatesAfter1stMove: Set[Candidates] = possibleCandidatesAfterNextPlayOn(corpus.initialCandidates)
//    println(s"...candidates recomputed")
//
//    println(s"possibleCandidatesAfter1stMove=${possibleCandidatesAfter1stMove.size}")
//
//    println(s"possibleWordSetsAfterFirstMove=${sizeHistogramOf(possibleCandidatesAfter1stMove.map(_.possibleWords), 50, 2315)}")
//    println(s"possibleDiscriminatorSetsAfterFirstMove=${sizeHistogramOf(possibleCandidatesAfter1stMove.map(_.discriminators), 200,12972)}")

  }
}

object AnalysisForCorpusWithGameMode {
  def obtainFor(corpusWithGameMode: CorpusWithGameMode): AnalysisForCorpusWithGameMode = {
    val corpus = corpusWithGameMode.corpus
    val grid: Array[Array[Byte]] = LocalFileCache.obtain(corpusWithGameMode.storageDir.resolve("grid.gz")) {
      for {
        candidateWord <- corpus.allWordsOrdered.toArray
      } yield {
        for (targetWord <- corpusWithGameMode.wordsRequiringEvaluationAsTargets)
          yield feedbackFor(candidateWord, targetWord).underlying
      }.toArray
    }
    AnalysisForCorpusWithGameMode(corpusWithGameMode, grid)
  }
}
