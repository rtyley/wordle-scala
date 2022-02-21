package com.madgag.wordle

object DriveWebPage {

  def javascriptCommandsFor(evidenceList: Seq[Evidence], nextGuess: Option[String] = None): String = {
    val indiciesToClear = (evidenceList.size + nextGuess.size) until 6
    ((for ((evidence, rowIndex) <- evidenceList.zipWithIndex) yield {
      s"setEvidence($rowIndex, '${evidence.guess}', '${evidence.wordFeedback.characters}');"
    }) ++ nextGuess.map(word => s"setWord(${evidenceList.size}, '$word');") ++ indiciesToClear.map(index => s"setWord($index, '');")).mkString
  }
}
