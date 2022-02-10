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
    forGameMode(Normal).bestInitial shouldBe WordGuessSum("laris".id, 50)
  }

//  it should "find the best candidate for a moderately large corpus in Normal mode" in {
//    val c = fullCorpus.reduceByAFactorOf(64)
//    val garpGarp = new GarpGarp(AnalysisForCorpusWithGameMode.obtainFor(c.withGameMode(Normal)))
//
//    val best = garpGarp.bestInitial
//    println(s"${c.allWordsOrdered(best.wordId)} ${best.guessSum} avg=${best.guessSum.toFloat/c.initialCandidates.possibleWords.size}")
//    println(garpGarp.newBestScoreCounter)
//    println(garpGarp.callsToFCounter)
//    println(s"${garpGarp.candidateSetsByInput.size} / ${garpGarp.computeNewCandidateSetsCounter} - requests=${garpGarp.newCandidateSetsRequestedCounter}")
//    println(Candidates.creationCounter)
//    println(Candidates.all.size) // Only 1971!?!
//  }
}
