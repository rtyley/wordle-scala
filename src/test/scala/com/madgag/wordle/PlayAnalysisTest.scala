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

  it should "match sonorous-chocolate for 1/800th corpus" in {
    given c: Corpus = Full.reducedByAFactorOf(800)
    forGameMode(Normal).bestInitial.value shouldBe WordGuessSum("aback".id, 5) // agrees with sonorous-chocolate
  }

  it should "match sonorous-chocolate for 1/400th corpus" in {
    given c: Corpus = Full.reducedByAFactorOf(400)
    forGameMode(Normal).bestInitial.value shouldBe WordGuessSum("aback".id, 11) // agrees with sonorous-chocolate
  }

  it should "match sonorous-chocolate for 1/200th corpus" in {
    given c: Corpus = Full.reducedByAFactorOf(200)
    forGameMode(Normal).bestInitial.value shouldBe WordGuessSum("scrag".id, 24) // agrees with sonorous-chocolate
  }

  it should "match sonorous-chocolate for 1/150th corpus" in {
    given c: Corpus = Full.reducedByAFactorOf(150)
    forGameMode(Normal).bestInitial.value shouldBe WordGuessSum("sayer".id, 32) // agrees with sonorous-chocolate
  }

  it should "match sonorous-chocolate for 1/100th corpus" in {
    given c: Corpus = Full.reducedByAFactorOf(100)
    println(c.id)
    forGameMode(Normal).bestInitial.value.guessSum shouldBe 50 // agrees with sonorous-chocolate
  }

  it should "match sonorous-chocolate for 1/90th corpus" in {
    given c: Corpus = Full.reducedByAFactorOf(90)
    c.writeOut()
    val boo = forGameMode(Normal).bestInitial.value
    /// println(boo.summary)
    boo shouldBe WordGuessSum("therm".id, 58) // should agree with sonorous-chocolate, surely?!
  }

  it should "match sonorous-chocolate for 1/80th corpus" in {
    given c: Corpus = Full.reducedByAFactorOf(80)
    c.writeOut()
    val boo = forGameMode(Normal).bestInitial.value
    println(boo.summary)
    boo.guessSum shouldBe 66 // should agree with sonorous-chocolate, surely?!
  }

  it should "match sonorous-chocolate for 1/50th corpus" in {
    given c: Corpus = Full.reducedByAFactorOf(50)
    c.writeOut()
    val boo = forGameMode(Normal).bestInitial.value
    println(boo.summary)
    boo.guessSum shouldBe 106 // should agree with sonorous-chocolate, surely?!
  }

  it should "do a sub-half-minute perf check" in {
    given Corpus = Full.reducedByAFactorOf(42)

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
