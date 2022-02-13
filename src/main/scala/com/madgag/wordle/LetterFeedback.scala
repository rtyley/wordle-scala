package com.madgag.wordle

enum LetterFeedback(val character: Char, val emoji: String, val ansiColor: fansi.Attr) {
  case Incorrect extends LetterFeedback('B',"⬜", fansi.Color.LightGray)
  case Misplaced extends LetterFeedback('Y',"🟨", fansi.Color.Yellow)
  case Correct   extends LetterFeedback('G',"🟩", fansi.Color.Green)
}

object LetterFeedback {
  def fromCharacter(character: Char): LetterFeedback =
    LetterFeedback.values.find(_.character == character).get
  
  def atStartOfString(stringStartingWithEmoji: String): LetterFeedback =
    LetterFeedback.values.find(lf => stringStartingWithEmoji.startsWith(lf.emoji)).get
}