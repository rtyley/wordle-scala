package com.madgag.wordle.players

import com.madgag.wordle.{Corpus, Game}
import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.PlayAnalysis.forGameMode
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HackyMcHackFaceTest extends AnyFlatSpec with Matchers with EitherValues {
  it should "play Wordles" in {
    given c:Corpus = Corpus.Full.reducedByAFactorOf(20)

    Game(c.commonWords.head, Normal).start.playWith(HackyMcHackFace)
  }
}