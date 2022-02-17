package com.madgag.wordle

import cats.*
import cats.data.*
import cats.implicits.*
import alleycats.std.set.*
import com.madgag.wordle.GameMode.{Hard, Normal}
import com.madgag.wordle.PlayAnalysisRedux.augmentWithRemainingCachedBetaThresholds
// import com.madgag.wordle.PlayAnalysis.{BrucieBonusAndIncompleteKnowledge, FResult, IncompleteKnowledge}
import com.madgag.wordle.approaches.tartan.{Candidates, FeedbackTable}
import com.madgag.wordle.wordsets.WordSet

import java.util.concurrent.ConcurrentMap
import java.util.concurrent.atomic.{AtomicLong, AtomicReference, LongAdder}
// import com.madgag.wordle.PlayAnalysis.*
import com.madgag.wordle.WordGuessSum.TotalFailure
import com.madgag.wordle.wordsets.partition.{FeedbackPartition, Partition}
import com.madgag.wordle.PlayAnalysisRedux.*

type FResult  = Either[Int, WordGuessSum]

object PlayAnalysisRedux {

  extension (fResult: FResult)
    def threshold: Int = fResult.map(_.guessSum).merge
    def add(c: Int) = fResult.map(_.addGuesses(c)).left.map(_ + c)

//  def forGameMode[P <: FPar](gameMode: GameMode)(using Corpus): PlayAnalysisRedux[P] =
//    new PlayAnalysisRedux(gameMode)

  def augmentWithRemainingCachedBetaThresholds[P <: FPar](
    incompleteKnowledges: Seq[IncompleteKnowledge[P]]
  ): Seq[BrucieBonusAndIncompleteKnowledge[P]] = {
    incompleteKnowledges.sorted(IncompleteKnowledge.OptimalOrdering).foldRight((0, List.empty[BrucieBonusAndIncompleteKnowledge[P]])) {
      case (incompleteKnowledge, (accBrucieBonus: Int, augmentedList: List[BrucieBonusAndIncompleteKnowledge[P]])) =>
        (
          accBrucieBonus + incompleteKnowledge.resultIsKnownToBeWorseThanThreshold.getOrElse(0),
          BrucieBonusAndIncompleteKnowledge(incompleteKnowledge, accBrucieBonus) :: augmentedList
        )
    }._2
  }
}

case class IncompleteKnowledge[P <: FPar](
  params: P,
  resultIsKnownToBeWorseThanThreshold: Option[Int]
)

object IncompleteKnowledge {
  val OptimalOrdering: Ordering[IncompleteKnowledge[_]] =
    Ordering.by(x => (-x.resultIsKnownToBeWorseThanThreshold.getOrElse(0), x.params.possibleWords.size))
}

case class BrucieBonusAndIncompleteKnowledge[P <: FPar](
  incompleteKnowledge: IncompleteKnowledge[P],
  brucieBonus: Int
)

sealed trait FPar {

  val guessIndex: Int

  val possibleWords: WordSet

  val nextGuessIndex: Int = guessIndex + 1

  val isDoomed: Boolean = guessIndex >= MaxGuesses || (nextGuessIndex==MaxGuesses && possibleWords.size > 1)
}

case class NormalFPar(guessIndex: Int, possibleWords: WordSet) extends FPar

case class HardFPar(
  guessIndex: Int,
  possibleWords: WordSet,
  permittedDiscriminators: WordSet
) extends FPar


sealed trait PlayAnalysisRedux[FP <: FPar](
  val gameMode: GameMode
)(using val corpus: Corpus) {
  val feedbackTable: FeedbackTable = FeedbackTable.obtainFor(gameMode)

  val initialParams: FP

  lazy val bestInitial: Option[WordGuessSum] = f(initialParams)

  val fResultsByFParams: ConcurrentMap[FP,FResult] = new java.util.concurrent.ConcurrentHashMap()


  /**
   * @param beta only pursue results that are better (lower) than this threshold - results that >= to this threshold
   *             are useless.
   * @return accurate result, if one could be found below the beta threshold, for the best word with it's guess-sum
   *         Some(TotalFailure) is a valid response, denoting that there was no way within the remaining number of
   *         guesses to correctly guess ALL possible words.
   *         If None is returned, that simply means we found no result below the threshold, and didn't search above
   *         the threshold.
   */
  def f(params: FP, beta: Int = 1000000): Option[WordGuessSum] = if (params.isDoomed) Some(TotalFailure) else {
    val possibleWords = params.possibleWords
    possibleWords.size match {
      case 1 => Some(WordGuessSum(possibleWords.head, 1))
      case 2 => Some(WordGuessSum(possibleWords.head, 3))
      case _ => cachedFResultsFor(params) match {
        case Some(Right(wordGuessSum)) => Some(wordGuessSum)
        case Some(Left(searchedThreshold)) if searchedThreshold >= beta => None
        case _ =>
          val candOutlooks: Seq[CandOutlook] = feedbackTable.orderedCandidateOutlooksFromEntireCorpusGiven(possibleWords)
          // .distinctBy(_.feedbackPartition.partition) // should be fine for Normal mode

          val newResult: FResult =
            candOutlooks.findBestWhileComparingAgainstBestSoFar[Int, WordGuessSum](beta - possibleWords.size, _.guessSum) {
              case (bestThresholdSoFar, candidateOutlook) => scoreIfBetterThan(bestThresholdSoFar, params, candidateOutlook)
          }.add(possibleWords.size)

          cacheResultOrUseBetterOne(params, newResult).toOption
      }
    }
  }

  private def cacheResultOrUseBetterOne(params: FP, newResult: FResult) = fResultsByFParams.merge(params, newResult, {
    case (a: Right[Int, WordGuessSum], _) => a
    case (_, b: Right[Int, WordGuessSum]) => b
    case (Left(a), Left(b)) => Left(Math.max(a, b))
  })

  private def cachedFResultsFor(fpar: FP) = Option[FResult](fResultsByFParams.get(fpar))


  def scoreIfBetterThan(thresholdToBeat: Int, fpar: FP, candOutlook: CandOutlook): Option[WordGuessSum] = {
    calculateRequiredGuesses(thresholdToBeat, candOutlook.t, candOutlook.feedbackPartition, fpar).map(score => WordGuessSum(candOutlook.t, score))
  }

  def updateParamsFor(fpar: FP, playedWordId: WordId, wordFeedback: WordFeedback, newPossibleWords: WordSet): FP

  /**
   * @return accurate result, if one could be found below the beta threshold, for the best word with it's guess-sum
   *         Some(TotalFailure) is a valid response, denoting that there was no way within the remaining number of
   *         guesses to correctly guess ALL possible words.
   *         If None is returned, that simply means we found no result below the threshold, and didn't search above
   *         the threshold.
   */
  private def calculateRequiredGuesses(thresholdToBeat: Int, playedWordId: WordId, partition: FeedbackPartition, oldParams: FP): Option[Int] = {
    val (incompleteKnowledges, knownGuessSums) = (for ((feedback, possibleWords) <- partition.wordSetByFeedback) yield {
      thresholdOrGuessSumFromCacheFor(updateParamsFor(oldParams, playedWordId, feedback, possibleWords))
    }).separate

    augmentWithRemainingCachedBetaThresholds(incompleteKnowledges).foldM(knownGuessSums.sum) {
      case (acc, brucieBonusAndIncompleteKnowledge) if acc < thresholdToBeat-brucieBonusAndIncompleteKnowledge.brucieBonus =>
        f(
          brucieBonusAndIncompleteKnowledge.incompleteKnowledge.params,
          thresholdToBeat - (acc + brucieBonusAndIncompleteKnowledge.brucieBonus)
        ).map(_.guessSum + acc)
      case _ => None
    }.filter(_ < thresholdToBeat)
  }

  private def thresholdOrGuessSumFromCacheFor(paramsIfWeGetThisSpecificFeedback: FP): Either[IncompleteKnowledge[FP], Int] = {
    cachedFResultsFor(paramsIfWeGetThisSpecificFeedback).fold(Left(IncompleteKnowledge(paramsIfWeGetThisSpecificFeedback, None))) {
      _.map(_.guessSum).left.map(threshold => IncompleteKnowledge(paramsIfWeGetThisSpecificFeedback, Some(threshold)))
    }
  }

}

class PlayAnalysisForNormalMode()(using corpus: Corpus) extends PlayAnalysisRedux[NormalFPar](Normal) {
  // val feedbackTable: FeedbackTable = FeedbackTable.obtainFor(gameMode)

  override val initialParams: NormalFPar = NormalFPar(0, corpus.initialCandidates.possibleWords)

  override def updateParamsFor(fpar: NormalFPar, playedWordId: WordId, wordFeedback: WordFeedback, newPossibleWords: WordSet): NormalFPar = {
    NormalFPar(fpar.nextGuessIndex, newPossibleWords)
  }
}

extension [E](items: Iterable[E])
  def findBestWhileComparingAgainstBestSoFar[T, A](
    startingThreshold: T, thresholdFor: A => T)(ifBetterThan: (T, E) => Option[A]): Either[T, A] = {
    items.foldLeft[Either[T, A]](Left(startingThreshold)) {
      case (bestSoFar, item) =>
        val currentThreshold = bestSoFar.map(thresholdFor).merge
        ifBetterThan(currentThreshold, item).fold(bestSoFar)(Right(_))
    }
  }