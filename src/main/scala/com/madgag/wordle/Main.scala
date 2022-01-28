package com.madgag.wordle

import com.madgag.wordle.Wordle.*
import com.madgag.scala.collection.decorators.*

import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Paths}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global
import scala.util.Random

@main def main() = {
  val corpus: Corpus = Corpus.load()

  println(corpus.grid)

  corpus.analyseGrid()

  val assay = ??? // Await.result(Assay.assayFor(PossibleWords.allWordsFrom(corpus)), Duration.Inf)

//  println("TOP\n"+assay.candidateWordAssaysSortedByScore.take(5))
//
//  println("BOTTOM\n"+assay.candidateWordAssaysSortedByScore.takeRight(5))

  def play(assay: Assay): Unit = {
    val popularWords: Set[Word] = assay.possibleWords.corpus.commonWords
    val targetWord: Word = popularWords.toSeq(Random.nextInt(popularWords.size))


    def takeAGuess(currentAssay: Assay, guessesTaken: Int): Unit = {
      // println(currentAssay.feedbackSpreads)
      // println(currentAssay.wordsGivingBestSpread.takeRight(5).mapV(_.toSeq.take(5)))

//      println(
//        currentAssay.candidateWordAssaysSortedByScore.take(3)
//          .map(p => p._1 +" "+ p._2.summariseFor(assay.possibleWords.corpus)).mkString("\n"))
      println(currentAssay.bitmapDiagnostic)
      val guess: Word = currentAssay.candidateWordAssaysSortedByScore.head._1
      val evidence = Evidence.evidenceFrom(guess, targetWord)
      // println("About to update assay...")
      val updatedAssay = currentAssay.updatedWith(evidence)
      val updatedGuessesTaken = guessesTaken + 1
      val mainReport = s"$updatedGuessesTaken. $evidence"
      if (!evidence.isSuccess && guessesTaken < 6) {
        println(s"$mainReport - ${updatedAssay.possibleWords.numPossibleWords} possible words left (${updatedAssay.possibleWords.possibleWords.take(9).mkString(", ")} ...), ${updatedAssay.numCandidateWords} candidates")
        takeAGuess(updatedAssay, updatedGuessesTaken)
      } else println(s"$mainReport !!!")
    }

    println(s"Target is '$targetWord', ${assay.possibleWords.numPossibleWords} possible words, ${assay.numCandidateWords} candidates")
    takeAGuess(assay, 0)
  }

  for (_ <- 1 to 20) {
    play(assay)
  }


}
