package com.madgag.wordle.approaches.feedbackarray

import com.madgag.wordle.{Assay, Evidence, PossibleWordId}
import com.madgag.wordle.Wordle.Word
import com.madgag.scala.collection.decorators._

case class FAAssay(possibleWords: Iterable[PossibleWordId], candidateAssayByWord: Map[Word, FACandidateAssay]) {
  def updatedWith(evidence: Evidence): FAAssay = {
    val candidateAssay = candidateAssayByWord(evidence.word)

    val updatedPossibleWords = for {
      (wordId, feedback) <- possibleWords.zip(candidateAssay.possibleWordFeedback) if feedback == evidence.wordFeedback
    } yield wordId

    candidateAssayByWord.mapV(_.)

    val assay = Assay(
      updatedPossibleWords,
      candidateAssayByWord.mapV(_.updateGiven(updatedPossibleWords.wordSet)).filter {
        case (_, ca) => !ca.canNotBeCorrectAndWouldRevealNoInformation
      }
    )
    // println("Update complete")
    assay
  }
}
