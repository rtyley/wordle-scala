package com.madgag.wordle.approaches.tartan

import com.madgag.scala.collection.decorators.*
import com.madgag.wordle.*
import com.madgag.wordle.GameMode.*
import com.madgag.wordle.evidence.WordFeedback.feedbackFor
import com.madgag.wordle.evidence.{Evidence, WordFeedback}
import com.madgag.wordle.util.LocalFileCache
import com.madgag.wordle.wordsets.*
import com.madgag.wordle.wordsets.partition.{FeedbackPartition, Partition}

import java.io.*
import java.nio.file.Files
import java.util.concurrent.ConcurrentMap
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Using

sealed trait FeedbackTable(
  val gameMode: GameMode,
  grid: Array[Array[Byte]]
)(using val corpus: Corpus) {

  val feedbackPartitionPool: FeedbackPartition.Pool = FeedbackPartition.Pool(Partition.Pool())

  def updatedPermittedWordsGiven(permittedWords: WordSet, wordId: WordId, wordFeedback: WordFeedback): WordSet

  def update(candidates: Candidates, evidence: Evidence): Candidates =
    possibleCandidateSetsIfCandidatePlayed(candidates, evidence.guess.id)(evidence.wordFeedback)

  // The results of this are independent of guess-number, so may be a good candidate for caching?
  def possibleCandidateSetsIfCandidatePlayed(candidates: Candidates, playedCandidateId: WordId): Map[WordFeedback,Candidates]

  def orderedCandidateOutlooksFor(h: Candidates): Seq[CandOutlook] = h.allWords.toSeq.map { t =>
    CandOutlook(t, partitionForCandidateGiven(h.possibleWords, t))
  }.sortBy(_.feedbackPartition.partition.evennessScore)

  def partitionForCandidateGiven(possibleWords: WordSet, playedCandidateId: WordId): FeedbackPartition = {
    val gridEntryForWord = grid(playedCandidateId)
    feedbackPartitionPool.intern(possibleWords.groupBy(wordId => new WordFeedback(gridEntryForWord(wordId))))
  }

  def wordsThatDoStillDiscriminate(
    possibleDiscriminators: Iterable[WordId], // Is it _best_ for it to be a WordSet?
    possibleWordsThatRemainPossible: WordSet
  ): WordSet = if (possibleWordsThatRemainPossible.sizeIs <= 2) WordSet.empty else WordSet.fromKnownDistinct(
    possibleDiscriminators
      .filter { wordId =>
        val gridEntryForWord = grid(wordId)
        val firstFeedback = gridEntryForWord(possibleWordsThatRemainPossible.head)
        possibleWordsThatRemainPossible.exists(gridEntryForWord(_) != firstFeedback) // re-introduce tail?
      }
  )
}

class AnalysisForCorpusWithNormalMode(
  grid: Array[Array[Byte]]
)(using corpus: Corpus) extends FeedbackTable(Normal, grid) {

  def updatedPermittedWordsGiven(permittedWords: WordSet, wordId: WordId, wordFeedback: WordFeedback): WordSet = permittedWords

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
  grid: Array[Array[Byte]]
)(using Corpus) extends FeedbackTable(Hard, grid) {

  def updatedPermittedWordsGiven(permittedWords: WordSet, playedWordId: WordId, wordFeedback: WordFeedback): WordSet = {
    val gridEntryForWord = grid(playedWordId)
    permittedWords.filter(wordId => gridEntryForWord(wordId) == wordFeedback.underlying)
  }

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

object FeedbackTable {

  val tableMap: ConcurrentMap[CorpusWithGameMode,FeedbackTable] = new java.util.concurrent.ConcurrentHashMap()

  def obtainFor(gameMode: GameMode)(using corpus: Corpus): FeedbackTable = {
    val corpusWithGameMode = CorpusWithGameMode(corpus, gameMode)
    tableMap.computeIfAbsent(corpusWithGameMode, { _ =>
      val grid: Array[Array[Byte]] = LocalFileCache.obtain(corpusWithGameMode.storageDir.resolve("grid.gz")) {
        for {
          candidateWord <- corpus.allWordsOrdered.toArray
        } yield {
          for (targetWord <- corpusWithGameMode.wordsRequiringEvaluationAsTargets)
            yield feedbackFor(candidateWord, targetWord).underlying
        }.toArray
      }

      corpusWithGameMode.gameMode match {
        case Normal => AnalysisForCorpusWithNormalMode(grid)
        case Hard => AnalysisForCorpusWithHardMode(grid)
      }
    })
  }
}
