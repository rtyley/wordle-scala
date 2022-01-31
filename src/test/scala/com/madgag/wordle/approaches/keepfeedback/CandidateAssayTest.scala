package com.madgag.wordle.approaches.keepfeedback

import com.madgag.wordle.Assay.assayFor
import CandidateAssay.OnlyCompleteSuccess
import com.madgag.wordle.Evidence.evidenceFrom
import com.madgag.wordle.LetterFeedback.*
import com.madgag.wordle.PossibleWords.allWordsFrom
import com.madgag.wordle.WordFeedback.{CompleteSuccess, feedbackFor}
import com.madgag.wordle.{Assay, Corpus}
import org.scalatest.Inspectors
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class CandidateAssayTest extends AsyncFlatSpec with Matchers {
  behavior of "CandidateAssay"

  it should "take a hint" in {
    val corpus = Corpus.fromAsteriskFormat(Seq("START*", "STORE*", "STORK*", "GLOVE*", "STORM"))
    assayFor(allWordsFrom(corpus)).map { initialAssay =>
      val updatedAssay = initialAssay.updatedWith(evidenceFrom("STORM", "STORE"))

      updatedAssay.candidateAssayByWord("STORE").possibleWordsByFeedback.size shouldBe 2
    }
  }
}