package com.madgag.wordle

enum LetterFeedback(val emoji: String, val ansiColor: fansi.Attr) {
  case Grey extends LetterFeedback("⬜", fansi.Color.LightGray)
  case Yellow extends LetterFeedback("🟨", fansi.Color.Yellow)
  case Green extends LetterFeedback("🟩", fansi.Color.Green)
}
