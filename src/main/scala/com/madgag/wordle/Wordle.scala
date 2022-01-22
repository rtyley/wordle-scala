package com.madgag.wordle

import com.madgag.scala.collection.decorators.*
import com.madgag.wordle.LetterFeedback.*
import scala.jdk.CollectionConverters._

import java.nio.file.{Files, Paths}

object Wordle {
  val WordLength = 5
  val WordIndices: Seq[Int] = 0 until WordLength

  type Letter = Char
  type Word = String

  // type WordFeedback = Seq[LetterFeedback]

  case class Assay(possibleWordsByFeedbackByCandidateWord: Map[Word,Map[WordFeedback,Set[Word]]])

  case class Foo(remainingActualLetters: Map[Letter, Int], misplacedLetterIndices: Set[Int] = Set.empty) {
    def attemptTake(letter: Letter, letterIndex: Int): Foo = {
      val quantityOfLetterAvailable = remainingActualLetters.getOrElse(letter, 0)
      if (quantityOfLetterAvailable <= 0) this else Foo(
        remainingActualLetters.updated(letter, quantityOfLetterAvailable - 1),
        misplacedLetterIndices + letterIndex
      )
    }
  }

  object Assay {
    def assayFor(candidateWords: Set[Word], possibleWords: Set[Word]): Assay = Assay((for {
      candidateWord <- candidateWords
    } yield candidateWord -> possibleWords.groupBy(possibleWord => WordFeedback.feedbackFor(candidateWord, possibleWord))).toMap
      // .mapV(_ => Map.empty)
    )
  }

}


