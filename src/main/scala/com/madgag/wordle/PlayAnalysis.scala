package com.madgag.wordle

import cats.*
import cats.data.*
import cats.implicits.*
import com.madgag.wordle.approaches.tartan.{FeedbackTable, Candidates}

import java.util.concurrent.atomic.LongAdder

case class WordGuessSum(wordId: WordId, guessSum: Int) extends Ordered[WordGuessSum] {
  override def compare(that: WordGuessSum): Int = guessSum.compareTo(that.guessSum)

  def addGuesses(x: Int) = copy(guessSum = guessSum + x)

  def word(using c: Corpus): Word = wordId.asWord

  def summary(using c: Corpus): String = {
    if (wordId>0) {
      s"$word $guessSum avg=${guessSum.toFloat/c.initialCandidates.possibleWords.size}"
    } else "*nothing found yet*"

  }

}

case class FParams(guessIndex: Int, h: Candidates)
case class FResult(beta: Int, wordGuessSum: WordGuessSum)

object PlayAnalysis {
  def forGameMode(gameMode: GameMode)(using c: Corpus): PlayAnalysis =
    new PlayAnalysis(FeedbackTable.obtainFor(CorpusWithGameMode(c, gameMode)))
}

class PlayAnalysis(
  feedbackTable: FeedbackTable
) {
  given Corpus = feedbackTable.corpus

  case class CandidatesPartition(possibleCandidates: Seq[Candidates]) {

    override val hashCode: Int = possibleCandidates.hashCode() // we rely on the hashcode a lot for `Set`s, so compute once...!

    val evennessScore: Int = possibleCandidates.map(c => c.possibleWords.size * c.possibleWords.size).sum

    // val borg: Seq[AtomicReference]

    def findRequiredGuessesWithPerfectPlay(thresholdToBeat: Int, nextGuessIndex: Int): Option[Int] = {
      possibleCandidates.foldM(0) {
        case (acc, candidates) if thresholdToBeat > acc =>
          Some(acc + f(nextGuessIndex, candidates, thresholdToBeat - acc).guessSum)
        case _ => None
      }.filter(_ < thresholdToBeat)
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

  lazy val bestInitial: WordGuessSum = f(0, feedbackTable.corpus.initialCandidates)

  /**
   *
   * @param beta only pursue results that are better (lower) than this threshold - results that >= to this threshold
   *             are useless.
   * @return accurate result, if one could be found below the beta threshold
   */
  def f(guessIndex: Int, h: Candidates, beta: Int = 1000000): WordGuessSum = if (guessIndex>=6) WordGuessSum(-1,1000000) else {
    callsToFCounter.increment()
    val numPossibleWords = h.possibleWords.size
    numPossibleWords match {
      case 0 => throw new IllegalStateException("Can't be!")
      case 1 => WordGuessSum(h.possibleWords.head,1)
      case 2 => WordGuessSum(h.possibleWords.head,3)
      case _ => {
        // Hit cache for FParams
        // If miss - calculate the stuff, store it
        // if hit - can we use the calculated data? YES if:
        // OLD-beta > NEW-beta --
        //   ie, we searched really hard, and permitted some really bad answers which you should now feel free to discard,
        //
        // or... old-result < NEW-beta
        // typically old-result (if found) < OLD-beta
        // if OLD-beta < NEW-beta, that doesn't mean that we can't use old-result, so long as it's < NEW-beta
        // if old-result > NEW-beta (a failed-to-find solution), surely we can still use old-result so long as OLD-beta > NEW-beta

        val fParams = FParams(guessIndex, h)
        val nextGuessIndex = guessIndex + 1

        val candidateOutlooks: Seq[CandidateOutlook] = h.allWords.toSeq.map { t =>
          outlookIfCandidatePlayed(h, t)
        }.distinctBy(_.candidatesPartition.hashCode).sortBy(_.candidatesPartition.evennessScore)

        candidateOutlooks.foldLeft(WordGuessSum(-1, beta)) {
          case (bestSoFar, candidateOutlook) =>
            val maybeSum = candidateOutlook.findCandidateScoringBetterThan(bestSoFar.guessSum, nextGuessIndex)
            if (guessIndex <= 1 ) {
              println(s"$guessIndex. ${candidateOutlook.t.asWord} $maybeSum - ${bestSoFar.summary}")
            }
            maybeSum.getOrElse(bestSoFar)
        }.addGuesses(h.possibleWords.size)
      }
    }
  }

  val candidateSetsByInput: java.util.concurrent.ConcurrentMap[(WordId, Candidates),CandidateOutlook] =
    new java.util.concurrent.ConcurrentHashMap()

  val newCandidateSetsRequestedCounter = new LongAdder
  val computeNewCandidateSetsCounter = new LongAdder

  private def outlookIfCandidatePlayed(h: Candidates, t: WordId): CandidateOutlook = {
    val key = (t, h)
    newCandidateSetsRequestedCounter.increment()

    candidateSetsByInput.computeIfAbsent(key, { _ =>
      computeNewCandidateSetsCounter.increment()
      CandidateOutlook(
        t,
        CandidatesPartition(
          (feedbackTable.possibleCandidateSetsIfCandidatePlayed(h, t) - WordFeedback.CompleteSuccess).values.toSeq.sortBy(_.possibleWords.size)
        )
      )
    })
  }
}

