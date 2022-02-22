package com.madgag.wordle

import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.approaches.tartan.FeedbackTable
import com.madgag.wordle.evidence.*
import com.madgag.wordle.evidence.WordFeedback.*
import org.scalatest.flatspec.AnyFlatSpec

import java.nio.file.Files

class SpectatorAnalystTest extends AnyFlatSpec {
  given corpus: Corpus = Corpus.Full
  val feedbackTable = FeedbackTable.obtainFor(Normal)


  it should "play Wordles" in {
    val analyst = SpectatorAnalyst(Normal)
    analyst.writeOutCsvFor(Seq(Evidence("grain", fb"⬜🟩🟩⬜⬜")), "draft")

    analyst.writeOutCsvFor(Seq(evidence.Evidence("grain", fb"⬜🟩🟩⬜⬜")), "celts")

  }
}
