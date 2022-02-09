package com.madgag.wordle.approaches.tartan

import com.madgag.scala.collection.decorators.*
import com.madgag.wordle.GameMode.*
import com.madgag.wordle.WordFeedback.feedbackFor
import com.madgag.wordle.*
import com.madgag.wordle.wordsets.*

import java.io.*
import java.nio.file.Files
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Using


sealed trait AnalysisForCorpusWithGameMode(
  corpus: Corpus,
  val gameMode: GameMode,
  grid: Array[Array[Byte]]
) {

  // The results of this are independent of guess-number, so may be a good candidate for caching?
  def possibleCandidateSetsIfCandidatePlayed(candidates: Candidates, playedCandidateId: WordId): Map[WordFeedback,Candidates]

  def wordsThatDoStillDiscriminate(
    possibleDiscriminators: Iterable[WordId], // Is it _best_ for it to be a WordSet?
    possibleWordsThatRemainPossible: WordSet
  ): WordSet = if (possibleWordsThatRemainPossible.sizeIs <= 2) WordSet.empty else WordSet.fromKnownDistinct(possibleDiscriminators.filter { wordId =>
    val gridEntryForWord = grid(wordId)
    val firstFeedback = gridEntryForWord(possibleWordsThatRemainPossible.head)
    // possibleWordsThatRemainPossible.view.tail.exists(gridEntryForWord(_) != firstFeedback)
    val bool = possibleWordsThatRemainPossible.exists {
      gridEntryForWord(_) != firstFeedback
    }
    bool
  })
}

class AnalysisForCorpusWithNormalMode(
  corpus: Corpus,
  grid: Array[Array[Byte]]
) extends AnalysisForCorpusWithGameMode(corpus, Normal, grid) {

  def possibleWordSetsOnCandidate(candidates: Candidates, playedCandidateId: WordId): Map[Byte, WordSet] = {
    val gridEntryForWord = grid(playedCandidateId)
    candidates.possibleWords.groupBy(gridEntryForWord(_))
  }

  def updateCandidatesRemovingPossibleWord(candidates: Candidates, wordId: WordId): Candidates = {
    updateCandidatesWithNewPossibleWordSet(candidates, candidates.possibleWords - wordId)
  }

  def updateCandidatesWithNewPossibleWordSet(candidates: Candidates, updatedPossibleWords: WordSet): Candidates = {
    updateCandidatesWith(
      candidates,
      updatedPossibleWords,
      candidates.possibleWords -- updatedPossibleWords // it would be good to have this op on WordSets optimised
    )
  }

  private def updateCandidatesWith(candidates: Candidates, possibleWordsThatRemainPossible: WordSet, possibleWordsThatBecameImpossible: WordSet) = {
    // if the `Candidates` for possibleWordsThatRemainPossible is already cached, return it (hard-mode, uh-oh?), skip next part
    val possibleDiscriminators =
      possibleWordsThatBecameImpossible.view ++ candidates.discriminators // we like these being in order, but don't *need* to do more than iterate over them
    // possibleWordsThatBecameImpossible ++ candidates.discriminators
    Candidates(
      possibleWords = possibleWordsThatRemainPossible,
      discriminators = wordsThatDoStillDiscriminate(possibleDiscriminators, possibleWordsThatRemainPossible)
    )
  }

  /**
   * NORMAL MODE: Partition possible words to comply with feedback - those that do not comply are possible
   * discriminators. Filter those and other discriminators to get discriminators that _do_ still discriminate!
   */
  override def possibleCandidateSetsIfCandidatePlayed(candidates: Candidates, playedCandidateId: WordId): Map[WordFeedback,Candidates] = {
    for ((feedbackByte, pws) <- possibleWordSetsOnCandidate(candidates, playedCandidateId)) yield {
      new WordFeedback(feedbackByte) -> updateCandidatesWithNewPossibleWordSet(candidates, pws)
    }
  }
}

class AnalysisForCorpusWithHardMode(
  val corpus: Corpus,
  grid: Array[Array[Byte]]
) extends AnalysisForCorpusWithGameMode(corpus, Hard, grid) {

  /**
   * HARD MODE: Trim both possible words & discriminators to comply with feedback
   */
  def possibleCandidateSetsIfCandidatePlayed(candidates: Candidates, playedWordId: WordId): Map[WordFeedback,Candidates] = {
    val gridEntryForWord = grid(playedWordId)
    val possibleWordSetsByFeedback = candidates.possibleWords.groupBy(gridEntryForWord(_))
    val discriminatorsByFeedback = candidates.discriminators.groupBy(gridEntryForWord(_)).withDefaultValue(WordSet.empty)
    for ((feedback, possibleWordSetGivenFeedback) <- possibleWordSetsByFeedback) yield new WordFeedback(feedback) -> Candidates(
      possibleWords = possibleWordSetGivenFeedback,
      discriminators = wordsThatDoStillDiscriminate(discriminatorsByFeedback(feedback), possibleWordSetGivenFeedback)
    )
  }
}



//class AnalysisForCorpusWithGameMode(corpusWithGameMode: CorpusWithGameMode, grid: Array[Array[Byte]]) {
//  val corpus: Corpus = corpusWithGameMode.corpus
//
//  def updateCandidatesRemovingPossibleWord(candidates: Candidates, wordId: WordId): Candidates = {
//    updateCandidatesWithNewPossibleWordSet(candidates, candidates.possibleWords - wordId)
//  }
//
//  def updateCandidatesWithNewPossibleWordSet(candidates: Candidates, updatedPossibleWords: SortedSet[WordId]): Candidates = {
//    updateCandidatesWith(candidates, updatedPossibleWords, candidates.possibleWords -- updatedPossibleWords)
//  }
//
//  def updateCandidatesWithEvidence(candidates: Candidates, evidenceWordId: WordId, evidenceFeedback: WordFeedback): Candidates = {
//    val (possibleWordsThatRemainPossible, possibleWordsThatBecameImpossible) = {
//      val gridEntryForEvidenceWord = grid(evidenceWordId)
//      candidates.possibleWords.partition(gridEntryForEvidenceWord(_) == evidenceFeedback.underlying)
//    }
//
//    updateCandidatesWith(candidates, possibleWordsThatRemainPossible, possibleWordsThatBecameImpossible)
//  }
//
//  private def updateCandidatesWith(candidates: Candidates, possibleWordsThatRemainPossible: SortedSet[WordId], possibleWordsThatBecameImpossible: SortedSet[WordId]) = {
//    // if the `Candidates` for possibleWordsThatRemainPossible is already cached, return it (hard-mode, uh-oh?), skip next part
//
//    val possibleDiscriminators = candidates.discriminators ++ possibleWordsThatBecameImpossible
//    Candidates(
//      possibleWords = possibleWordsThatRemainPossible,
//      discriminators = wordsThatDoStillDiscriminate(possibleDiscriminators, possibleWordsThatRemainPossible)
//    )
//  }
//
//
//
//  def possibleCandidateSetsAfter(candidates: Candidates, playedCandidateId: WordId): Set[Candidates] = {
//    /**
//     * NORMAL: Partition possible words to comply with feedback, those that do not comply are possible
//     * discriminators. Filter those and other discriminators to ensure they still discriminate!
//     *
//     * HARD: Trim both possible words & discriminators to comply with feedback
//     */
//
//    possibleWordSetsOnCandidate(candidates, playedCandidateId).map(pws =>
//      updateCandidatesWithNewPossibleWordSet(candidates, pws)
//    )
//  }
//
//
//
//
//  def analyseGrid() = {
//    val strategies = Seq(BitSetSize, BestOfStrategy(Seq(ShortArraySize, InvertedShortArraySize)))
//
//    def sizeHistogramOf(idSets: Set[Set[WordId]], bucketSize: Int = 200, maxSetSize: Int): String = {
//      val setQuantityBySize: SortedMap[Int, Int] = SortedMap.from(idSets.groupBy(_.size).mapV(_.size))
//      val bucketSizeByBucket: SortedMap[Int, Int] = SortedMap.from(setQuantityBySize.groupBy {
//        case (setSize, quantity) => (setSize / bucketSize) * bucketSize
//      }.mapV(_.values.sum))
//
//      val histogram = {
//        val maxBucketSize = bucketSizeByBucket.values.max
//        (for ((bucket, bucketSize) <- bucketSizeByBucket) yield {
//          f"$bucket%5d : $bucketSize%6d ${Seq.fill(60 * bucketSize / maxBucketSize)("■").mkString}"
//        }).mkString("\n")
//      }
//      val storageSummary = strategies.map { strategy =>
//        f"${strategy.totalSizeFor(setQuantityBySize, maxSetSize)}%12d : $strategy"
//      }.mkString("\n")
//      s"${idSets.size} sets \n$histogram\nTotal storage required:\n$storageSummary"
//    }
//
////    val allPossibleSplitsForCandidates: Set[Set[SortedSet[WordId]]] = possibleWordSetsForCandidates(corpus.initialCandidates)
////    println(allPossibleSplitsForCandidates.size)
//
////    val possibleCandidatesAfter1stMove: Set[Candidates] = possibleCandidatesAfterNextPlayOn(corpus.initialCandidates)
////    println(s"...candidates recomputed")
////
////    println(s"possibleCandidatesAfter1stMove=${possibleCandidatesAfter1stMove.size}")
////
////    println(s"possibleWordSetsAfterFirstMove=${sizeHistogramOf(possibleCandidatesAfter1stMove.map(_.possibleWords), 50, 2315)}")
////    println(s"possibleDiscriminatorSetsAfterFirstMove=${sizeHistogramOf(possibleCandidatesAfter1stMove.map(_.discriminators), 200,12972)}")
//
//  }
//}

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

    corpusWithGameMode.gameMode match {
      case Normal => AnalysisForCorpusWithNormalMode(corpus, grid)
      case Hard => AnalysisForCorpusWithHardMode(corpus, grid)
    }
  }
}
