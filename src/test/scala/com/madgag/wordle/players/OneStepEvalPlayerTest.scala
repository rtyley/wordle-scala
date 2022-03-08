package com.madgag.wordle.players

import com.madgag.wordle.Corpus.Full
import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.{Corpus, Game, GameMode, Strategy, WordlePlayer}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OneStepEvalPlayerTest extends AnyFlatSpec with Matchers with EitherValues {
  it should "play Wordle with full corpus, ha!" in {
    given corpus: Corpus = Full
    val player: WordlePlayer = OneStepEvalPlayer.playing(Normal)

    println(Game(corpus.commonWords.head, Normal).playWith(player).value.text)

    val rootChoice = Strategy.fromPlayer(player, Normal)

    Game.totalGuessSumFor(StrategyTreePlayer(rootChoice).playing, Normal) shouldBe 8060
  }
}
