package com.madgag.wordle

import com.madgag.wordle.Corpus.{Full, fromAsteriskFormat}
import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.PlayAnalysis.forGameMode
import com.madgag.wordle.approaches.tartan.{Candidates, FeedbackTable}
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PlayAnalysisTest extends AnyFlatSpec with Matchers {

  it should "give the correct answer, 1 word corpus" in {
    given Corpus = fromAsteriskFormat("aback*")
    forGameMode(Normal).bestInitial shouldBe WordGuessSum("aback".id, 1)
  }

  it should "give the correct answer, 2 word corpus" in {
    given Corpus = fromAsteriskFormat("aback*", "apple*")
    forGameMode(Normal).bestInitial.guessSum shouldBe 3
  }

  it should "give the correct answer, 3/4 word corpus" in {
    given Corpus = fromAsteriskFormat("aback*", "defer*", "gully*", "angry")
    forGameMode(Normal).bestInitial shouldBe WordGuessSum("angry".id, 6)
  }

  it should "give the correct answer, for a quick check on 1% of the full corpus!" in {
    given Corpus = Full.reducedByAFactorOf(100)
    forGameMode(Normal).bestInitial.guessSum shouldBe 50
  }

  it should "give the correct answer, for a sub-half-minute perf check" in {
    given Corpus = Full.reducedByAFactorOf(61)

    val playAnalysis = forGameMode(Normal)
    val best = playAnalysis.bestInitial

    println(best.summary)

    println(playAnalysis.callsToFCounter)
    println(playAnalysis.newBestScoreCounter)
    println(s"${playAnalysis.candidateSetsByInput.size} / ${playAnalysis.computeNewCandidateSetsCounter} - requests=${playAnalysis.newCandidateSetsRequestedCounter}")
    println(Candidates.creationCounter)
    println(Candidates.all.size) // Only 1971!?!
  }

}
