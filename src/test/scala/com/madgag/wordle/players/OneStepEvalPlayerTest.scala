package com.madgag.wordle.players

import com.madgag.wordle.*
import com.madgag.wordle.Corpus.Full
import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.{Corpus, Game, GameMode, Strategy, WordFeedback, WordlePlayer}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OneStepEvalPlayerTest extends AnyFlatSpec with Matchers with EitherValues {
  it should "deal with 'fancy' getting BGGBB" in {
    given corpus: Corpus = Full
    val player: WordlePlayer = OneStepEvalPlayer.playing(Normal)

    val stateAfterFancy = player.start.updateWith(Evidence("fancy", WordFeedback.fromChars("BGGBB")))
    println(stateAfterFancy.move)
  }

  it should "play Wordle with full corpus, ha!" in {
    given corpus: Corpus = Full
    val player: WordlePlayer = OneStepEvalPlayer.playing(Normal)

    println(Game(corpus.commonWords.head, Normal).start.playWith(player).text)

    val rootChoice = Strategy.fromPlayer(player, Normal)

    Game.totalGuessSumFor(StrategyTreePlayer(rootChoice).playing, Normal) shouldBe 8060
  }
}
