package com.madgag.wordle

import com.madgag.wordle.Wordle.Assay
import com.madgag.wordle.Evidence.evidenceFrom
import com.madgag.wordle.LetterFeedback.*
import com.madgag.wordle.WordFeedback.feedbackFor
import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class CandidateAssayTest extends AnyWordSpec with Matchers {
  "Wasawa" should {
    "boom" in {
      val corpus = Corpus.fromAsteriskFormat(Seq("START*", "STORE*", "STORK*", "GLOVE*", "STORM"))
      val possibleWords = PossibleWords.allWordsFrom(corpus)
      val startAssay = Await.result(Assay.assayFor(possibleWords), Duration.Inf)

      val updatedAssay = startAssay.updateWith(evidenceFrom("STORM", "STORE"))

      updatedAssay.possibleWordsByFeedbackByCandidateWord("STORE")
        .possibleActualWordsByFeedback(WordFeedback.CompleteSuccess).bitSet.map(corpus.orderedCommonWords) shouldBe Set("STORE")
      println(updatedAssay)
    }

  }
}