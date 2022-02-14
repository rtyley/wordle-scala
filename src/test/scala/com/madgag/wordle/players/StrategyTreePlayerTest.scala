package com.madgag.wordle.players

import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.StrategyExample.given_Corpus
import com.madgag.wordle.{Corpus, Game, StrategyExample}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StrategyTreePlayerTest extends AnyFlatSpec with Matchers {
  it should "play Wordles" in {
    import StrategyExample.*
    given corpus: Corpus = StrategyExample.given_Corpus

    val player = StrategyTreePlayer(exampleStrategyTree)

    Game.totalGuessSumFor(player, Normal) shouldBe 50
  }
}
