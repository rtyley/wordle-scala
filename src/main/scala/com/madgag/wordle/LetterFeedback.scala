package com.madgag.wordle

enum LetterFeedback(val emoji: String, val ansiColor: fansi.Attr) {
  case Incorrect extends LetterFeedback("⬜", fansi.Color.LightGray)
  case Misplaced extends LetterFeedback("🟨", fansi.Color.Yellow)
  case Correct extends LetterFeedback("🟩", fansi.Color.Green)
}

object LetterFeedback {
  def atStartOfString(stringStartingWithEmoji: String): LetterFeedback =
    LetterFeedback.values.find(lf => stringStartingWithEmoji.startsWith(lf.emoji)).get
}