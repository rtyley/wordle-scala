package com.madgag.wordle

import com.madgag.scala.collection.decorators.*
import com.madgag.wordle.LetterFeedback.*
import com.madgag.wordle.WordFeedback.CompleteSuccess
import com.madgag.wordle.CandidateAssay.OnlyCompleteSuccess
import com.madgag.wordle.PossibleWordSetStore.idFor
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

    protected val allBitMaps: Seq[BitSet] =
      possibleWordsByFeedbackByCandidateWord.values.toSeq.flatMap(_.possibleActualWordsByFeedback.values.map(PossibleWordSetStore.wordSetFor))
    private val uniqueBitMaps: Set[BitSet] = allBitMaps.toSet
    protected val numDifferentBitMaps = uniqueBitMaps.size
    protected val numDifferentBitMapHashCodes = allBitMaps.map(_.hashCode).toSet.size

    val possibleWordSetsCardinality: SortedMap[Int, String] =
      SortedMap.from(uniqueBitMaps.groupUp(_.size) {
        bitMaps =>
//          val avgBytes: Float = bitMaps.map(_.getSizeInBytes).sum.toFloat / bitMaps.size
//          val avgBytesPerEntry = avgBytes / bitMaps.head.getCardinality
          // f"${bitMaps.size} $avgBytes%2.1f $avgBytesPerEntry%2.1f per"
          bitMaps.size.toString
      })

    val bitmapDiagnostic =
      s"${allBitMaps.size} bitmaps - distinct=$numDifferentBitMaps hashCodes=$numDifferentBitMapHashCodes possibleWordSetsCardinality=${possibleWordSetsCardinality.mkString(",")}"

    lazy val candidateWordAssaysSortedByMaxPossibleWordSetSize: SortedMap[Int, Iterable[Word]] =
      SortedMap.from(possibleWordsByFeedbackByCandidateWord.groupMap(_._2.maxPossibleWordsSize)(_._1))

    lazy val candidateWordAssaysSortedByScore: Seq[(Word, CandidateAssay)] =
      possibleWordsByFeedbackByCandidateWord.toSeq.sortBy {
        case (word, ca) =>
        (ca.score, !possibleWords.idsOfPossibleWords.contains(possibleWords.corpus.orderedCommonWords.indexOf(word)))
      }

    def updateWith(evidence: Evidence): Assay = {
      val updatedPossibleWords = possibleWords.copy(
        idsOfPossibleWords =
          possibleWordsByFeedbackByCandidateWord(evidence.word).possibleActualWordsByFeedback(evidence.wordFeedback)
      )
      Assay(
        updatedPossibleWords,
        possibleWordsByFeedbackByCandidateWord.mapV(_.updateGiven(updatedPossibleWords.idsOfPossibleWords)).filter {
          case (_, ca) => !ca.canNotBeCorrectAndWouldRevealNoInformation
        }
      )
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


