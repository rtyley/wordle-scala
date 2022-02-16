package com.madgag.wordle.players

import com.google.common.io.Resources
import com.madgag.wordle.*
import com.madgag.wordle.Corpus.{Full, getClass}
import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.StrategyExample.given_Corpus
import com.madgag.wordle.{Corpus, Game, StrategyExample}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.jdk.CollectionConverters.*

import java.nio.charset.StandardCharsets.UTF_8

class StrategyTreePlayerTest extends AnyFlatSpec with Matchers {
  it should "play Wordles" in {
    import StrategyExample.*
    given Corpus = StrategyExample.given_Corpus

    val player = StrategyTreePlayer(exampleStrategyTree).playing

    Game.totalGuessSumFor(player, Normal) shouldBe 50
  }

  it should "play Wordles that we get wrong" in {
    given Corpus = Full.reducedByAFactorOf(90)
    val rootChoice = Strategy.treeFrom(
      Resources.asCharSource(getClass.getResource("/sonorous-chocolate/tree.corpus-26-of-145-words__F1978AB0.Normal.txt"), UTF_8)
        .readLines().asScala.toSeq)

    val player = StrategyTreePlayer(rootChoice).playing

    Game.totalGuessSumFor(player, Normal) shouldBe 58
  }
}
