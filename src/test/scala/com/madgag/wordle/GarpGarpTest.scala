package com.madgag.wordle

import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.approaches.tartan.AnalysisForCorpusWithGameMode
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GarpGarpTest extends AnyFlatSpec with Matchers {

  val c = Corpus.load().reduceByAFactorOf(100)
  val garpGarp = new GarpGarp(AnalysisForCorpusWithGameMode.obtainFor(c.withGameMode(Normal)))

  it should "find the best candidate for a moderately large corpus in Normal mode" in {
    val best = garpGarp.f(0, c.initialCandidates)
    println(s"${c.allWordsOrdered(best.wordId)} ${best.guessSum} avg=${best.guessSum.toFloat/c.initialCandidates.possibleWords.size}")
    println(garpGarp.newBestScoreCounter)
    println(garpGarp.callsToFCounter)
  }
}