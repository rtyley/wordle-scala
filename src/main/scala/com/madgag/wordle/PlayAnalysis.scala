package com.madgag.wordle

import cats.*
import cats.data.*
import cats.implicits.*
import alleycats.std.set._
import com.madgag.wordle.PlayAnalysis.{BrucieBonusAndIncompleteFParamsKnowledge, FResult, IncompleteFParamsKnowledge}
import com.madgag.wordle.approaches.tartan.{Candidates, FeedbackTable}

import java.util.concurrent.atomic.{AtomicLong, AtomicReference, LongAdder}

case class WordGuessSum(wordId: WordId, guessSum: Int) extends Ordered[WordGuessSum] {
  override def compare(that: WordGuessSum): Int = guessSum.compareTo(that.guessSum)

  def addGuesses(x: Int) = copy(guessSum = guessSum + x)

  def word(using c: Corpus): Word = wordId.asWord

  def summary(using c: Corpus): String = {
    if (wordId>=0) {
      s"$word $guessSum avg=${guessSum.toFloat/c.initialCandidates.possibleWords.size}"
    } else "*nothing found yet*"

  }
}

object WordGuessSum {
  val TotalFailure = WordGuessSum(-1,1000000)
}

case class FParams(guessIndex: Int, h: Candidates)
// case class FResult(beta: Int, wordGuessSum: WordGuessSum)

object PlayAnalysis {

  type FResult  = Either[Int, WordGuessSum]

  def forGameMode(gameMode: GameMode)(using Corpus): PlayAnalysis =
    new PlayAnalysis(FeedbackTable.obtainFor(gameMode))

  case class CandidatesPartitionPlayCache(thresholdToBeat: Int, guessSum: Option[Int])

  case class IncompleteFParamsKnowledge(
    params: FParams,
    resultKnownToBeWorseThanThreshold: Option[Int]
  )

  object IncompleteFParamsKnowledge {
    val OptimalOrdering: Ordering[IncompleteFParamsKnowledge] =
      Ordering.by(x => (-x.resultKnownToBeWorseThanThreshold.getOrElse(0), x.params.h.possibleWords.size))
  }

  case class BrucieBonusAndIncompleteFParamsKnowledge(
    incompleteFParamsKnowledge: IncompleteFParamsKnowledge,
    brucieBonus: Int
  )
}

case class PlayAnalysis(
  feedbackTable: FeedbackTable
) {
  given Corpus = feedbackTable.corpus

  object CandidatesPartition {
    private val candidatesPartitionBySet: java.util.concurrent.ConcurrentMap[Set[Candidates],CandidatesPartition] =
      new java.util.concurrent.ConcurrentHashMap()
      
    def stored: Int = candidatesPartitionBySet.size()
    
    def intern(possibleCandidates: Set[Candidates]): CandidatesPartition = 
      candidatesPartitionBySet.computeIfAbsent(possibleCandidates, { _ => CandidatesPartition(possibleCandidates) })
  }
  
  case class CandidatesPartition(possibleCandidates: Set[Candidates]) {

    override val hashCode: Int = possibleCandidates.hashCode() // we rely on the hashcode a lot for `Set`s, so compute once...!

    val evennessScore: Int = possibleCandidates.map(c => c.possibleWords.size * c.possibleWords.size).sum

    val calculationCache: Seq[AtomicReference[PlayAnalysis.CandidatesPartitionPlayCache]] =
      Seq.fill(MaxGuesses)(new AtomicReference(PlayAnalysis.CandidatesPartitionPlayCache(0, None)))

    def findRequiredGuessesWithPerfectPlay(thresholdToBeat: Int, nextGuessIndex: Int): Option[Int] = {
//       Given a guess index, can we return a cached answer?
//       if we cached with a higher thresholdToBeat, we can answer with the cached answer
//       otherwise we must calculate and store
      val atomicRef: AtomicReference[PlayAnalysis.CandidatesPartitionPlayCache] = calculationCache(nextGuessIndex)
      val cachedValue: PlayAnalysis.CandidatesPartitionPlayCache = atomicRef.get
      if (cachedValue.thresholdToBeat>thresholdToBeat) cachedValue.guessSum else {
        val newGuessSum = calculateRequiredGuesses(thresholdToBeat, nextGuessIndex)
        atomicRef.updateAndGet { oldCachedValue =>
          if (thresholdToBeat > oldCachedValue.thresholdToBeat) PlayAnalysis.CandidatesPartitionPlayCache(thresholdToBeat,newGuessSum) else oldCachedValue
        }.guessSum
      }
    }

    /**
     * @return accurate result, if one could be found below the beta threshold, for the best word with it's guess-sum
     *         Some(TotalFailure) is a valid response, denoting that there was no way within the remaining number of
     *         guesses to correctly guess ALL possible words.
     *         If None is returned, that simply means we found no result below the threshold, and didn't search above
     *         the threshold.
     */
    private def calculateRequiredGuesses(thresholdToBeat: Int, nextGuessIndex: Int): Option[Int] = {
      val (incompleteKnowledges, knownGuessSums) = possibleCandidates.map {
        h =>
          val params = FParams(nextGuessIndex, h)
          cachedFResultsFor(params).fold(Left(IncompleteFParamsKnowledge(params, None))) { result =>
            result.map(_.guessSum).left.map(threshold => IncompleteFParamsKnowledge(params, Some(threshold)))
          }
      }.separate

      val incompleteKnowledgesSorted: Seq[IncompleteFParamsKnowledge] =
        incompleteKnowledges.toSeq.sorted(IncompleteFParamsKnowledge.OptimalOrdering)

      augmentWithRemainingCachedBetaThresholds(incompleteKnowledgesSorted).foldM(knownGuessSums.sum) {
        case (acc, bruciesBonusAndIncompleteKnowledge) if acc < thresholdToBeat-bruciesBonusAndIncompleteKnowledge.brucieBonus =>
          f(
            nextGuessIndex,
            bruciesBonusAndIncompleteKnowledge.incompleteFParamsKnowledge.params.h,
            thresholdToBeat - (acc + bruciesBonusAndIncompleteKnowledge.brucieBonus)
          ).map(_.guessSum + acc)
        case _ => None
      }.filter(_ < thresholdToBeat)
    }

    private def augmentWithRemainingCachedBetaThresholds(
      incompleteKnowledgesSorted: Seq[IncompleteFParamsKnowledge]
    ): Seq[BrucieBonusAndIncompleteFParamsKnowledge] = {
      incompleteKnowledgesSorted.foldRight((0, List.empty[BrucieBonusAndIncompleteFParamsKnowledge])) {
        case (incompleteKnowledge, (accBrucieBonus: Int, augmentedList: List[BrucieBonusAndIncompleteFParamsKnowledge])) =>
          (
            accBrucieBonus + incompleteKnowledge.resultKnownToBeWorseThanThreshold.getOrElse(0),
            BrucieBonusAndIncompleteFParamsKnowledge(incompleteKnowledge, accBrucieBonus) :: augmentedList
          )
      }._2
    }
  }

  case class CandidateOutlook(t: WordId, candidatesPartition: CandidatesPartition) {
    def findCandidateScoringBetterThan(thresholdToBeat: Int, nextGuessIndex: Int): Option[WordGuessSum] = {
      candidatesPartition.findRequiredGuessesWithPerfectPlay(thresholdToBeat, nextGuessIndex).map {
        newBestScore => WordGuessSum(t, newBestScore)
      }
    }
  }

  val newBestScoreCounter = new LongAdder()
  val callsToFCounter = new LongAdder()

  val fResultsByFParams: java.util.concurrent.ConcurrentMap[FParams,FResult] =
    new java.util.concurrent.ConcurrentHashMap()

  lazy val bestInitial: Option[WordGuessSum] = f(0, feedbackTable.corpus.initialCandidates)

  /**
   * @param beta only pursue results that are better (lower) than this threshold - results that >= to this threshold
   *             are useless.
   * @return accurate result, if one could be found below the beta threshold, for the best word with it's guess-sum
   *         Some(TotalFailure) is a valid response, denoting that there was no way within the remaining number of
   *         guesses to correctly guess ALL possible words.
   *         If None is returned, that simply means we found no result below the threshold, and didn't search above
   *         the threshold.
   */
  def f(guessIndex: Int, h: Candidates, beta: Int = 1000000): Option[WordGuessSum] = {
    val numPossibleWords = h.possibleWords.size
    val nextGuessIndex = guessIndex + 1
    if (guessIndex>=MaxGuesses || (nextGuessIndex==MaxGuesses && numPossibleWords>1)) Some(WordGuessSum.TotalFailure) else {
      callsToFCounter.increment()
      numPossibleWords match {
        case 0 => throw new IllegalStateException("Can't be!")
        case 1 => Some(WordGuessSum(h.possibleWords.head,1))
        case 2 => Some(WordGuessSum(h.possibleWords.head,3))
        case _ =>
          val fParams = FParams(guessIndex, h)

          cachedFResultsFor(fParams) match {
            case Some(Right(wordGuessSum)) => Some(wordGuessSum)
            case Some(Left(searchedThreshold)) if searchedThreshold >= beta => None
            case _ =>
              val candidateOutlooks: Seq[CandidateOutlook] =
                orderedCandidateOutlooksFor(h).distinctBy(_.candidatesPartition.hashCode)

              val newResult = candidateOutlooks.foldLeft[FResult](Left(beta-h.possibleWords.size)) {
                case (bestSoFar, candidateOutlook) =>
                  val maybeSum = candidateOutlook.findCandidateScoringBetterThan(bestSoFar.map(_.guessSum).merge, nextGuessIndex)
//                  if (guessIndex <= 1 ) {
//                    println(s"$guessIndex. ${candidateOutlook.t.asWord} $maybeSum - ${bestSoFar.summary}")
//                  }
                  maybeSum.map(Right(_)).getOrElse(bestSoFar)
              }.map(_.addGuesses(h.possibleWords.size)).left.map(_ => beta)

              fResultsByFParams.merge(fParams, newResult, {
                case (a: Right[Int,WordGuessSum], _) => a
                case (_ ,b: Right[Int,WordGuessSum]) => b
                case (Left(a) ,Left(b)) => Left(Math.max(a,b))
              }).toOption
          }

      }
    }
  }

  /* Is an Option enough?!?
  * If we have a cached entry where WordGuessSum is populated, we don't *care* what beta the answer
  was calculated with - it's accurate. However, if we have cached a 'None' (ie 'there was no answer
  under this beta') then we _do_ care what the beta is - we need to know whether we've searched for
  answers as big and bad as the current beta we're using.
  The outcome is three different things:
  1. Here is the accurate wordguesssum
  2. the wordguesssum is unknown, but higher than a specific threshold (that threshold may be lower than what we need...
  3. I either:
     - know nothing about this wordguesssum - it's not been calculated/cached yet!
     - only know that it's higher than a very low threshold...

  Actions taken:
  1. Add the wordguesssum to the accumulator
  2. Short-circuit summin process!
  3. Calculate the wordguesssum, add it to the accumulator
  */
  private def cachedFResultsFor(fParams: FParams): Option[FResult] =
    Option(fResultsByFParams.get(fParams))

  def orderedCandidateOutlooksFor(h: Candidates): Seq[CandidateOutlook] = h.allWords.toSeq.map { t =>
    outlookIfCandidatePlayed(h, t)
  }.sortBy(_.candidatesPartition.evennessScore)

  val candidateSetsByInput: java.util.concurrent.ConcurrentMap[(WordId, Candidates),CandidateOutlook] =
    new java.util.concurrent.ConcurrentHashMap()

  val newCandidateSetsRequestedCounter = new LongAdder
  val computeNewCandidateSetsCounter = new LongAdder

  private def outlookIfCandidatePlayed(h: Candidates, t: WordId): CandidateOutlook = {
    val key = (t, h) // Need quick key, eg: val key: Long = (t<<32) + h.hashCode
    newCandidateSetsRequestedCounter.increment()

    candidateSetsByInput.computeIfAbsent(key, { _ =>
      computeNewCandidateSetsCounter.increment()
      CandidateOutlook(
        t,
        CandidatesPartition.intern(
          (feedbackTable.possibleCandidateSetsIfCandidatePlayed(h, t) - WordFeedback.CompleteSuccess).values.toSet
        )
      )
    })
  }
}

