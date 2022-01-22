package com.madgag.wordle

import com.madgag.scala.collection.decorators.*
import com.madgag.wordle.LetterFeedback.*
import org.roaringbitmap.RoaringBitmap

import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Paths}
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global

object Wordle {
  val WordLength = 5
  val WordIndices: Seq[Int] = 0 until WordLength

  type Letter = Char
  type Word = String

  // type WordFeedback = Seq[LetterFeedback]

  case class Assay(possibleWordsByFeedbackByCandidateWord: Map[Word,Map[WordFeedback,RoaringBitmap]])

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
    def assayFor(possibleWords: PossibleWords): Future[Assay] = {
      for {
        possibleWordsWithFeedbackByCandidateWord <- Future.traverse(possibleWords.corpus.words) { candidateWord =>
          Future(candidateWord -> evaluateCandidate(candidateWord, possibleWords))
        }
      } yield Assay(possibleWordsWithFeedbackByCandidateWord.toMap)
    }

    private def evaluateCandidate(candidateWord: Word, possibleWords: PossibleWords): Map[WordFeedback, RoaringBitmap] = {
      possibleWords.idsOfPossibleWords.asScala.map(_.toInt).groupUp { idOfPossibleWork =>
        WordFeedback.feedbackFor(candidateWord, possibleWords.corpus.orderedWords(idOfPossibleWork))
      }(bigOleThing => RoaringBitmap.bitmapOf(bigOleThing.toArray: _*))
    }
  }

}


