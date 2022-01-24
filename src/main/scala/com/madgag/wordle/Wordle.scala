package com.madgag.wordle

import com.madgag.scala.collection.decorators.*
import com.madgag.wordle.LetterFeedback.*
import com.madgag.wordle.WordFeedback.CompleteSuccess
import com.madgag.wordle.CandidateAssay.OnlyCompleteSuccess
import com.madgag.wordle.PossibleWordSetStore.{idFor, wordSetFor}
import org.roaringbitmap.RoaringBitmap

import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Path, Paths}
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import scala.collection.immutable.{BitSet, SortedMap}

object Wordle {
  val WordLength = 5
  val WordIndices: Seq[Int] = 0 until WordLength

  type Letter = Char
  type Word = String

  // type WordFeedback = Seq[LetterFeedback]

  case class Assay(possibleWords: PossibleWords, possibleWordsByFeedbackByCandidateWord: Map[Word,CandidateAssay]) {
    val numCandidateWords: Int = possibleWordsByFeedbackByCandidateWord.size

    val feedbackSpreads: SortedMap[Int, Int] =
      SortedMap.from(possibleWordsByFeedbackByCandidateWord.values.map(_.possibleActualWordsByFeedback.size).groupUp(identity)(_.size))

    val wordsGivingBestSpread: SortedMap[Int, Set[Word]] =
      SortedMap.from(possibleWordsByFeedbackByCandidateWord.groupUp(_._2.possibleActualWordsByFeedback.size)(_.keySet))

    protected val allBitMaps: Seq[WordSetId] =
      possibleWordsByFeedbackByCandidateWord.values.toSeq.flatMap(_.possibleActualWordsByFeedback.values)
    private val uniqueBitMaps: Set[WordSetId] = allBitMaps.toSet
    protected val numDifferentBitMaps: Int = uniqueBitMaps.size

    lazy val possibleWordSetsCardinality: SortedMap[Int, String] =
      SortedMap.from(uniqueBitMaps.groupUp(wsId => wordSetFor(wsId).size) {
        bitMaps =>
//          val avgBytes: Float = bitMaps.map(_.getSizeInBytes).sum.toFloat / bitMaps.size
//          val avgBytesPerEntry = avgBytes / bitMaps.head.getCardinality
          // f"${bitMaps.size} $avgBytes%2.1f $avgBytesPerEntry%2.1f per"
          bitMaps.size.toString
      })

    val bitmapDiagnostic =
      s"${allBitMaps.size} wordsets - distinct=$numDifferentBitMaps"

    lazy val candidateWordAssaysSortedByMaxPossibleWordSetSize: SortedMap[Int, Iterable[Word]] =
      SortedMap.from(possibleWordsByFeedbackByCandidateWord.groupMap(_._2.maxPossibleWordsSize)(_._1))

    lazy val candidateWordAssaysSortedByScore: Seq[(Word, CandidateAssay)] =
      possibleWordsByFeedbackByCandidateWord.toSeq.sortBy {
        case (word, ca) =>
        (ca.score, !possibleWords.possibleWords.contains(word))
      }

    def updateWith(evidence: Evidence): Assay = {
      val updatedPossibleWords = possibleWords.copy(
        wordSetId =
          possibleWordsByFeedbackByCandidateWord(evidence.word).possibleActualWordsByFeedback(evidence.wordFeedback)
      )
      val assay = Assay(
        updatedPossibleWords,
        possibleWordsByFeedbackByCandidateWord.mapV(_.updateGiven(updatedPossibleWords.wordSetId)).filter {
          case (_, ca) => !ca.canNotBeCorrectAndWouldRevealNoInformation
        }
      )
      // println("Update complete")
      assay
    }

    def store(): Unit = {
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
        possibleWordsWithFeedbackByCandidateWord <- Future.traverse(possibleWords.corpus.allWordsEvenTheUncommonOnes) { candidateWord =>
          Future(candidateWord -> evaluateCandidate(candidateWord, possibleWords))
        }
      } yield {
        val assay = Assay(possibleWords, possibleWordsWithFeedbackByCandidateWord.toMap)
        assay.store()
        assay
      }
    }

    private def evaluateCandidate(candidateWord: Word, possibleWords: PossibleWords): CandidateAssay = CandidateAssay(
      possibleWords.idsOfPossibleWords.groupBy { idOfPossibleWord =>
        WordFeedback.feedbackFor(candidateWord, possibleWords.corpus.orderedCommonWords(idOfPossibleWord))
      }.mapV(naiveSet => idFor(BitSet.fromSpecific(naiveSet)))
    )
  }

}


