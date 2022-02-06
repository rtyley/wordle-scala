package com.madgag.wordle

case class Evidence(word: Word, wordFeedback: WordFeedback) {
  val ansiColouredString: fansi.Str = (for ((attr, char) <- wordFeedback.toSeq.map(_.ansiColor).zip(word)) yield {
    attr(char.toString)
  }).reduce(_ ++ _)

  val isSuccess: Boolean = wordFeedback.isSuccess

  override val toString: String = ansiColouredString.toString
}

object Evidence {
  def evidenceFrom(candidate: Word, actual: Word) = Evidence(
    candidate,
    WordFeedback.feedbackFor(candidate, actual)
  )
}