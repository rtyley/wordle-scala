package com.madgag.wordle

import com.madgag.wordle.Evidence.*
import com.madgag.wordle.WordFeedback.feedbackFor
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EvidenceTest extends AnyFlatSpec with Matchers with EitherValues {
  it should "create nice colouring" in {
    val evidence = Evidence.evidenceFrom("sassy", "grass")

    println(evidence)
  }

  it should "be useful in working out what words are possible" in {
    val evidence = Evidence("early", WordFeedback("🟨⬜⬜🟩⬜"))

    Corpus.Full.commonWords.count(_.compliesWith(evidence)) shouldBe 30
  }
}
