package com.madgag.wordle.strategy_files

import com.madgag.wordle.*
import com.madgag.wordle.Corpus.{Full, fromAsteriskFormat}
import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.WordFeedback.{CompleteSuccess, fromChars}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LineTest extends AnyFlatSpec with Matchers with OptionValues {

  given Corpus = Full.reducedByAFactorOf(100)

  it should "recognise the first line" in {
    val line = Line("laris BBBBY1 pesto GGGGG2")
    line shouldBe Line(
      guessIndexForHeadFeedbackOrRootWordId = Right("laris".id),
      tailPairs = Seq((fromChars("BBBBY"), "pesto".id))
    )
    line.guessIndexForHeadFeedback shouldBe 0 // this codebase uses a zero-based guess index
  }

  it should "recognise a later line first line" in {
    val line = Line("                   YBBBB2 human GGGGG3")
    line shouldBe Line(
      guessIndexForHeadFeedbackOrRootWordId = Left(1),
      tailPairs = Seq((fromChars("YBBBB"), "human".id))
    )
    line.guessIndexForHeadFeedback shouldBe 1
  }

  it should "handle the lines where not much happens" in {
    val line = Line("                   GGGGG2")
    line shouldBe Line(
      guessIndexForHeadFeedbackOrRootWordId = Left(1),
      tailPairs = Seq.empty
    )
    line.guessIndexForHeadFeedback shouldBe 1
  }
}
