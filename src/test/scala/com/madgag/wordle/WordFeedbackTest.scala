package com.madgag.wordle

import com.madgag.wordle.Evidence.evidenceFrom
import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.madgag.wordle.LetterFeedback.*
import com.madgag.wordle.WordFeedback.feedbackFor

class WordFeedbackTest extends AnyWordSpec with Matchers {
  "WordFeedback" should {
    "represent the 5 letter-feedback values we get on a Wordle word" in {
      WordFeedback(Correct, Misplaced, Incorrect, Incorrect, Incorrect).emojis shouldBe "🟩🟨⬜⬜⬜"
      WordFeedback(Correct, Correct,   Correct,   Correct,   Incorrect).emojis shouldBe "🟩🟩🟩🟩⬜"
    }

    "round-trip emoji representation" in {
      WordFeedback("⬜🟨⬜🟩⬜").emojis shouldBe "⬜🟨⬜🟩⬜"
      WordFeedback("🟩⬜⬜⬜🟨").emojis shouldBe "🟩⬜⬜⬜🟨"
      WordFeedback("🟩🟩⬜🟨⬜").emojis shouldBe "🟩🟩⬜🟨⬜"
    }

    "round-trip from a sequence of letter-feedback values to a single-byte representation" in {
      val justOneGreenAtStart = Seq(Correct, Incorrect, Incorrect, Incorrect, Incorrect)
      WordFeedback(justOneGreenAtStart).toSeq shouldBe justOneGreenAtStart

      val justTwoYellowAtStart = Seq(Misplaced, Misplaced, Incorrect, Incorrect, Incorrect)
      WordFeedback(justTwoYellowAtStart).toSeq shouldBe justTwoYellowAtStart
    }

    "have a convenient 'success' value" in {
      WordFeedback.CompleteSuccess.emojis shouldBe "🟩🟩🟩🟩🟩"
    }

    "correctly calculate expected word-feedback for candidate words against a target word" in {
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