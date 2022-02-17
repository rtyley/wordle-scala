package com.madgag.wordle.players

import com.madgag.wordle.*
import com.madgag.wordle.Corpus.Full
import com.madgag.wordle.{Corpus, Game, StrategyExample}
import com.madgag.wordle.GameMode.Normal
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HackyMcHackFaceTest extends AnyFlatSpec with Matchers with EitherValues {
  it should "play Wordle with full corpus, ha!" in {
    given corpus: Corpus = Full
    val player = HackyMcHackFace.playing(Normal)

    println(Game(corpus.commonWords.head, Normal).start.playWith(player).text)
  }

  it should "play Wordle with the 1% corpus" in {
    given corpus: Corpus = StrategyExample.given_Corpus

    val player = HackyMcHackFace.playing(Normal)

    Game(corpus.commonWords.head, Normal).start.playWith(player)

    Game.totalGuessSumFor(player, Normal) shouldBe 50
  }

  it should "play Wordles we used to get wrong..." in {
    given Corpus = Full.reducedByAFactorOf(90)

    val player = HackyMcHackFace.playing(Normal)

    Game.totalGuessSumFor(player, Normal) shouldBe 58
  }
}