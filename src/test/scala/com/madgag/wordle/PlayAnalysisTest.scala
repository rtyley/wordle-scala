package com.madgag.wordle

import com.madgag.wordle.Corpus.{Full, fromAsteriskFormat}
import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.PlayAnalysis.forGameMode
import com.madgag.wordle.approaches.tartan.{Candidates, FeedbackTable}
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PlayAnalysisTest extends AnyFlatSpec with Matchers with OptionValues {

  it should "give the correct answer, 1 word corpus" in {
    given Corpus = fromAsteriskFormat("aback*")
    forGameMode(Normal).bestInitial.value shouldBe WordGuessSum("aback".id, 1)
  }

  it should "give the correct answer, 2 word corpus" in {
    given Corpus = fromAsteriskFormat("aback*", "apple*")
    forGameMode(Normal).bestInitial.value.guessSum shouldBe 3
  }

  it should "give the correct answer, 3/4 word corpus" in {
    given Corpus = fromAsteriskFormat("aback*", "defer*", "gully*", "angry")
    forGameMode(Normal).bestInitial.value shouldBe WordGuessSum("angry".id, 6)
  }

  it should "acknowledge when the best strategy is just simple guessing" in {
    given Corpus = fromAsteriskFormat("eight*", "fight*", "sight*", "might*","night*", "right*")
    forGameMode(Normal).bestInitial.value.guessSum shouldBe 21
  }

  it should "acknowledge when everything is hopeless" in {
    given Corpus = fromAsteriskFormat("eight*", "fight*", "sight*", "might*","night*", "right*", "light*")
    forGameMode(Normal).bestInitial.exists(_.wordId>0) shouldBe false // maybe should be TotalFailure?
  }

  it should "give the correct answer, for a quick check on 1% of the full corpus!" in {
    given Corpus = Full.reducedByAFactorOf(100)
    forGameMode(Normal).bestInitial.value.guessSum shouldBe 50
  }

  it should "give the correct answer, for a quick check on 2% of the full corpus!" in {
    given Corpus = Full.reducedByAFactorOf(50)
    val wordGuessSum: WordGuessSum = forGameMode(Normal).bestInitial.value
    println(wordGuessSum.summary)
    wordGuessSum.guessSum shouldBe 102
  }

  it should "give the correct answer, for a sub-half-minute perf check" in {
    given Corpus = Full.reducedByAFactorOf(38)

    val playAnalysis = forGameMode(Normal)
    val best = playAnalysis.bestInitial.value

    println(best.summary)

    println(s"Candidates Partitions stored = ${playAnalysis.CandidatesPartition.stored}")
    println(playAnalysis.callsToFCounter)
    println(playAnalysis.newBestScoreCounter)
    println(s"${playAnalysis.candidateSetsByInput.size} / ${playAnalysis.computeNewCandidateSetsCounter} - requests=${playAnalysis.newCandidateSetsRequestedCounter}")
    println(Candidates.creationCounter)
    println(Candidates.stored) // Only 1971!?!
  }

}
