package com.madgag.wordle

import com.madgag.wordle.WordFeedback.feedbackFor

case class Evidence(word: Word, wordFeedback: WordFeedback) {
  val ansiColouredString: fansi.Str = (for ((attr, char) <- wordFeedback.toSeq.map(_.ansiColor).zip(word)) yield {
    attr(char.toString)
  }).reduce(_ ++ _)

  val isSuccess: Boolean = wordFeedback.isSuccess

  override val toString: String = ansiColouredString.toString
}

object Evidence {
  def evidenceFrom(candidate: Word, actual: Word) = Evidence(candidate, feedbackFor(candidate, actual))

  extension (word: Word)
    def compliesWith(evidence: Evidence): Boolean = feedbackFor(evidence.word, word) == evidence.wordFeedback
}