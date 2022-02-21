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
    val evidence = Evidence("early", fb"🟨⬜⬜🟩⬜")

    Corpus.Full.commonWords.count(_.compliesWith(evidence)) shouldBe 30
  }

  it should "be able to work out the prospects of 'draft' after 'grain' gets two greens" in {
    val evidence = Evidence("grain", fb"⬜🟩🟩⬜⬜")

    val possibleWordsAfter1stGuess: Set[Word] = Corpus.Full.commonWords.filter(_.compliesWith(evidence))

    println(s"There are ${possibleWordsAfter1stGuess.size} possible target words after '$evidence'\n")


    val guess2: Word = "draft"

    val possibleWordsGroupedByFeedback: Map[WordFeedback, Set[Word]] =
      possibleWordsAfter1stGuess.groupBy(possibleWord => feedbackFor(guess2, possibleWord))

    println(s"There are ${possibleWordsGroupedByFeedback.size} differing feedbacks possible after 2nd guess '$guess2'\n")

    println(possibleWordsGroupedByFeedback)

  }
}
