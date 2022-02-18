package com.madgag.wordle.approaches.tartan

import com.madgag.wordle.*
import com.madgag.wordle.Game
import com.madgag.wordle.GameMode.Normal
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FeedbackTableTest extends AnyFlatSpec with Matchers {

  given corpus: Corpus = Corpus.Full
  val feedbackTable = FeedbackTable.obtainFor(Normal)

}
