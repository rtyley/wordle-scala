package com.madgag.wordle.approaches.feedbackarray

import com.madgag.wordle.WordFeedback

case class FACandidateAssay(possibleWordFeedback: Array[WordFeedback]) {
  def updatedPossibleWordSetGiven(feedback: WordFeedback): Iterable[Int] = {
    possibleWordFeedback.zipWithIndex.filter(_._1 == feedback).map(_._2)
  }
}
