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

sealed trait FeedbackTable(
  val corpus: Corpus,
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
    possibleWordsThatRemainPossible.exists(gridEntryForWord(_) != firstFeedback) // re-introduce tail?
  })
}

class AnalysisForCorpusWithNormalMode(
  corpus: Corpus,
  grid: Array[Array[Byte]]
) extends FeedbackTable(corpus, Normal, grid) {

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
  corpus: Corpus,
  grid: Array[Array[Byte]]
) extends FeedbackTable(corpus, Hard, grid) {

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
  def obtainFor(corpusWithGameMode: CorpusWithGameMode): FeedbackTable = {
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
