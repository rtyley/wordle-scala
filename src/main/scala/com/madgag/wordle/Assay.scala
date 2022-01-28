package com.madgag.wordle

import com.madgag.wordle.Wordle.Word
import com.madgag.scala.collection.decorators._
import scala.collection.immutable.SortedMap
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global

case class Assay(possibleWords: PossibleWords, candidateAssayByWord: Map[Word,CandidateAssay]) {
  
  val numCandidateWords: Int = candidateAssayByWord.size

  val feedbackSpreads: SortedMap[Int, Int] =
    SortedMap.from(candidateAssayByWord.values.map(_.possibleWordsByFeedback.size).groupUp(identity)(_.size))

  val wordsGivingBestSpread: SortedMap[Int, Set[Word]] =
    SortedMap.from(candidateAssayByWord.groupUp(_._2.possibleWordsByFeedback.size)(_.keySet))

  protected val allBitMaps: Seq[WordSet] =
    candidateAssayByWord.values.toSeq.flatMap(_.possibleWordsByFeedback.values)
  private val uniqueBitMaps: Set[WordSet] = allBitMaps.toSet
  protected val numDifferentBitMaps: Int = uniqueBitMaps.size

  val bitmapDiagnostic =
    s"${allBitMaps.size} wordsets - distinct=$numDifferentBitMaps"

  lazy val candidateWordAssaysSortedByMaxPossibleWordSetSize: SortedMap[Int, Iterable[Word]] =
    SortedMap.from(candidateAssayByWord.groupMap(_._2.maxPossibleWordsSize)(_._1))

  lazy val candidateWordAssaysSortedByScore: Seq[(Word, CandidateAssay)] =
    candidateAssayByWord.toSeq.sortBy {
      case (word, ca) =>
        (ca.score, !possibleWords.possibleWords.contains(word))
    }

  def updatedWith(evidence: Evidence): Assay = {
    val updatedPossibleWords = possibleWords.copy(
      wordSet =
        candidateAssayByWord(evidence.word).possibleWordsByFeedback(evidence.wordFeedback)
    )
    val assay = Assay(
      updatedPossibleWords,
      candidateAssayByWord.mapV(_.updateGiven(updatedPossibleWords.wordSet)).filter {
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

object Assay {
  def assayFor(possibleWords: PossibleWords): Future[Assay] = {
//    for {
//      possibleWordsWithFeedbackByCandidateWord <- Future.traverse(possibleWords.corpus.allWordsEvenTheUncommonOnes) { candidateWord =>
//        Future(candidateWord -> evaluateCandidate(candidateWord, possibleWords))
//      }
//    } yield {
//      val assay = Assay(possibleWords, possibleWordsWithFeedbackByCandidateWord.toMap)
//      assay.store()
//      assay
//    }
    ???
  }

//  private def evaluateCandidate(candidateWord: Word, possibleWords: PossibleWords): CandidateAssay = CandidateAssay(
//    possibleWords.wordSet.bitSet.groupBy { idOfPossibleWord =>
//      WordFeedback.feedbackFor(candidateWord, possibleWords.corpus.orderedCommonWords(idOfPossibleWord))
//    }.mapV(PossibleWordSetStore.intern)
//  )
}
