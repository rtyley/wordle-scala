package com.madgag.wordle

import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.approaches.tartan.{AnalysisForCorpusWithGameMode, Candidates}
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GarpGarpTest extends AnyFlatSpec with Matchers {

  private val fullCorpus: Corpus = Corpus.load()
  val c = fullCorpus.reduceByAFactorOf(64)
  val garpGarp = new GarpGarp(AnalysisForCorpusWithGameMode.obtainFor(c.withGameMode(Normal)))

  it should "give the correct answer, for a quick check on 1% of the full corpus!" in {
    val c = fullCorpus.reduceByAFactorOf(100)
    val garpGarp = new GarpGarp(AnalysisForCorpusWithGameMode.obtainFor(c.withGameMode(Normal)))
    garpGarp.f(0, c.initialCandidates) shouldBe WordGuessSum(c.idFor("laris"), 50)
  }

//  it should "find the best candidate for a moderately large corpus in Normal mode" in {
//    val c = fullCorpus.reduceByAFactorOf(64)
//    val garpGarp = new GarpGarp(AnalysisForCorpusWithGameMode.obtainFor(c.withGameMode(Normal)))
//
//    val best = garpGarp.f(0, c.initialCandidates)
//    println(s"${c.allWordsOrdered(best.wordId)} ${best.guessSum} avg=${best.guessSum.toFloat/c.initialCandidates.possibleWords.size}")
//    println(garpGarp.newBestScoreCounter)
//    println(garpGarp.callsToFCounter)
//    println(s"${garpGarp.candidateSetsByInput.size} / ${garpGarp.computeNewCandidateSetsCounter} - requests=${garpGarp.newCandidateSetsRequestedCounter}")
//    println(Candidates.creationCounter)
//    println(Candidates.all.size) // Only 1971!?!
//  }
}
