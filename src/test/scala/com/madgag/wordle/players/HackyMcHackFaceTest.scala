package com.madgag.wordle.players

import com.madgag.wordle.*
import com.madgag.wordle.Corpus.Full
import com.madgag.wordle.{Corpus, Game, StrategyExample}
import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.PlayAnalysis.forGameMode
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HackyMcHackFaceTest extends AnyFlatSpec with Matchers with EitherValues {
//  it should "play Wordle with full corpus, ha!" in {
//    given corpus: Corpus = Full
//
//    Game(corpus.commonWords.head, Normal).start.playWith(HackyMcHackFace)
//  }

  it should "play Wordles" in {
    import StrategyExample.*
    given corpus: Corpus = StrategyExample.given_Corpus

    val player = HackyMcHackFace.playing(Normal)

    Game(corpus.commonWords.head, Normal).playWith(player)

    Game.totalGuessSumFor(player, Normal) shouldBe 50
  }

  it should "play Wordles we definitely get wrong" in {
    given corpus: Corpus = Full.reducedByAFactorOf(90)

    val player = HackyMcHackFace.playing(Normal)

    val rootChoice = Strategy.fromPlayer(player, Normal)

    println(rootChoice)

    Game.totalGuessSumFor(player, Normal) shouldBe 58
  }
}