package com.madgag.wordle.players

import com.madgag.wordle.{Corpus, Game, StrategyExample}
import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.PlayAnalysis.forGameMode
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HackyMcHackFaceTest extends AnyFlatSpec with Matchers with EitherValues {
  it should "play Wordles" in {
    import StrategyExample.*
    given corpus: Corpus = StrategyExample.given_Corpus

    Game(corpus.commonWords.head, Normal).start.playWith(HackyMcHackFace)

    Game.totalGuessSumFor(HackyMcHackFace, Normal) shouldBe 50
  }
}