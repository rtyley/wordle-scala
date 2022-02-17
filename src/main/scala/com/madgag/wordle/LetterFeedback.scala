package com.madgag.wordle

enum LetterFeedback(val character: Char, val emoji: String, val ansiColor: fansi.Attr, jsName: String) {
  case Incorrect extends LetterFeedback('B',"⬜", fansi.Color.LightGray, "absent")
  case Misplaced extends LetterFeedback('Y',"🟨", fansi.Color.LightYellow, jsName="present")
  case Correct   extends LetterFeedback('G',"🟩", fansi.Color.LightGreen, jsName="correct")
}

object LetterFeedback {
  def fromCharacter(character: Char): LetterFeedback =
    LetterFeedback.values.find(_.character == character).get
  
  def atStartOfString(stringStartingWithEmoji: String): LetterFeedback =
    LetterFeedback.values.find(lf => stringStartingWithEmoji.startsWith(lf.emoji)).get
}