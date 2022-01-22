package com.madgag.wordle

import com.madgag.scala.collection.decorators.*
import com.madgag.wordle.LetterFeedback.*
import org.roaringbitmap.RoaringBitmap

import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Path, Paths}
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global

object Wordle {
  val WordLength = 5
  val WordIndices: Seq[Int] = 0 until WordLength

  type Letter = Char
  type Word = String

  // type WordFeedback = Seq[LetterFeedback]

  case class CandidateAssay(possibleActualWordsByFeedback: Map[WordFeedback,RoaringBitmap]) {
    lazy val score: Int = possibleActualWordsByFeedback.values.map { bitMap =>
      val cardinality = bitMap.getCardinality
      cardinality * cardinality
    }.sum

    def updateGiven(newSuperSetOfPossibleWords: RoaringBitmap): CandidateAssay = CandidateAssay(
      possibleActualWordsByFeedback.view.mapValues {
        originalWordPossibleGivenFeedback
         => RoaringBitmap.and(originalWordPossibleGivenFeedback,newSuperSetOfPossibleWords)
      }.toMap
    )

    lazy val totalBitMapSize: Long = possibleActualWordsByFeedback.values.map(_.getSizeInBytes).sum
  }

  case class Assay(possibleWords: PossibleWords, possibleWordsByFeedbackByCandidateWord: Map[Word,CandidateAssay]) {
    lazy val candidateWordAssaysSortedByScore: Seq[(Word, Int)] = possibleWordsByFeedbackByCandidateWord.toSeq.map(t => t._1 -> t._2.score).sortBy(_._2)

    def updateWith(evidence: Evidence): Assay = {
      val updatedPossibleWords = possibleWords.copy(
        idsOfPossibleWords = possibleWordsByFeedbackByCandidateWord(evidence.word).possibleActualWordsByFeedback(evidence.wordFeedback)
      )
      Assay(
        possibleWords,
        possibleWordsByFeedbackByCandidateWord.mapV(_.updateGiven(updatedPossibleWords.idsOfPossibleWords))
      )
    }

    lazy val totalBitMapSize: Long = possibleWordsByFeedbackByCandidateWord.values.map(_.totalBitMapSize).sum

    def store(): Unit = {
      println(s"Storing... total bitmap size is $totalBitMapSize")

      println("Stored")
    }

  }

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
      } yield {
        val assay = Assay(possibleWords, possibleWordsWithFeedbackByCandidateWord.toMap)
        assay.store()
        assay
      }
    }

    private def evaluateCandidate(candidateWord: Word, possibleWords: PossibleWords): CandidateAssay = CandidateAssay(
      possibleWords.idsOfPossibleWords.asScala.map(_.toInt).groupUp { idOfPossibleWork =>
        WordFeedback.feedbackFor(candidateWord, possibleWords.corpus.orderedWords(idOfPossibleWork))
      }(bigOleThing => RoaringBitmap.bitmapOf(bigOleThing.toArray: _*))
    )
  }

}


