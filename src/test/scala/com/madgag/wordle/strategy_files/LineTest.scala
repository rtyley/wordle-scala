package com.madgag.wordle.strategy_files

import com.madgag.wordle.*
import com.madgag.wordle.Corpus.{Full, fromAsteriskFormat}
import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.PlayAnalysis.forGameMode
import com.madgag.wordle.WordFeedback.{CompleteSuccess, fromChars}
import com.madgag.wordle.{Corpus, WordFeedback, WordGuessSum}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LineTest extends AnyFlatSpec with Matchers with OptionValues {

  given Corpus = Full.reducedByAFactorOf(100)

  it should "recognise the first line" in {
    Line("laris BBBBY1 pesto GGGGG2") shouldBe Line(
      headFeedbackGuessIndexOrRootWordId = Right("laris".id),
      tailPairs = Seq((fromChars("BBBBY"), "pesto".id))
    )
  }

  it should "recognise a later line first line" in {
    Line("                   YBBBB2 human GGGGG3") shouldBe Line(
      headFeedbackGuessIndexOrRootWordId = Left(1),
      tailPairs = Seq((fromChars("YBBBB"), "human".id))
    )
  }
}
