package com.madgag.wordle

import com.madgag.wordle.Evidence.evidenceFrom
import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.madgag.wordle.LetterFeedback.*
import com.madgag.wordle.WordFeedback.feedbackFor

class WordFeedbackTest extends AnyWordSpec with Matchers {
  "WordFeedback" should {
    "recognise success" in {
      WordFeedback.CompleteSuccess.emojis shouldBe "🟩🟩🟩🟩🟩"

      WordFeedback(Correct, Misplaced, Incorrect, Incorrect, Incorrect).emojis shouldBe "🟩🟨⬜⬜⬜"
      WordFeedback(Correct, Correct, Correct, Correct, Incorrect).emojis shouldBe "🟩🟩🟩🟩⬜"
      WordFeedback("⬜🟨⬜🟩⬜").emojis shouldBe "⬜🟨⬜🟩⬜"
      WordFeedback("🟩⬜⬜⬜🟨").emojis shouldBe "🟩⬜⬜⬜🟨"
    }

    "roundtrip" in {
      val justGreenAtStart = Seq(Correct, Incorrect, Incorrect, Incorrect, Incorrect)
      WordFeedback(justGreenAtStart).toSeq shouldBe justGreenAtStart

      val justTwoYellowAtStart = Seq(Misplaced, Misplaced, Incorrect, Incorrect, Incorrect)
      WordFeedback(justTwoYellowAtStart).toSeq shouldBe justTwoYellowAtStart
    }

    "give good feedback on target word 'PERKY'" in {
      for ((candidateWord, expectedFeedback) <- Seq(
        "RAISE" -> "🟨⬜⬜⬜🟨",
        "PRANK" -> "🟩🟨⬜⬜🟨",
        "FRANK" -> "⬜🟨⬜⬜🟨",
        "DAILY" -> "⬜⬜⬜⬜🟩",
        "WORTH" -> "⬜⬜🟩⬜⬜",
        "SHIFT" -> "⬜⬜⬜⬜⬜"
      )) {
        feedbackFor(candidateWord, "PERKY").emojis shouldBe expectedFeedback
      }
    }

    "do examples found in http://sonorouschocolate.com/notes/index.php?title=The_best_strategies_for_Wordle" in {
      feedbackFor("SILLY", "HOTEL").emojis shouldBe "⬜⬜🟨⬜⬜"
      feedbackFor("SILLY", "DAILY").emojis shouldBe "⬜🟨⬜🟩🟩"
    }
  }
}