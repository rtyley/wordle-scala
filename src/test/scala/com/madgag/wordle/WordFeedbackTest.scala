package com.madgag.wordle

import com.madgag.wordle.Evidence.evidenceFrom
import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.madgag.wordle.LetterFeedback.*
import com.madgag.wordle.WordFeedback.feedbackFor

class WordFeedbackTest extends AnyWordSpec with Matchers {
  "Goosg" should {
    "roundtrip" in {
      val justGreenAtStart = Seq(Green, Grey, Grey, Grey, Grey)
      WordFeedback(justGreenAtStart).toSeq shouldBe justGreenAtStart

      val justTwoYellowAtStart = Seq(Yellow, Yellow, Grey, Grey, Grey)
      WordFeedback(justTwoYellowAtStart).toSeq shouldBe justTwoYellowAtStart
    }

    "give good feedback on a word" in {
      feedbackFor("BREAD", "DAILY").emojis shouldBe "⬜⬜⬜🟨🟨"

      println(feedbackFor("BAEEB", "ABTEY").emojis)
      println(evidenceFrom("CHORE", "CHICK").ansiColouredString)
      println(evidenceFrom("CHOCK", "CHICK").ansiColouredString)
    }
  }
}