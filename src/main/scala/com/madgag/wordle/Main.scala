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

  val assay = Await.result(Assay.assayFor(PossibleWords.allWordsFrom(corpus)), Duration.Inf)

  // println(assay)
  println(assay.possibleWordsByFeedbackByCandidateWord.size)
  println(assay.possibleWordsByFeedbackByCandidateWord.head)
  println("TOP\n"+assay.candidateWordAssaysSortedByScore.take(5))

  println("BOTTOM\n"+assay.candidateWordAssaysSortedByScore.takeRight(5))

  def play(assay: Assay): Unit = {
    val popularWords: Set[Word] = assay.possibleWords.corpus.popularWords
    val targetWord: Word = popularWords.toSeq(Random.nextInt(popularWords.size))
    println(s"Target is $targetWord")
    def takeAGuess(currentAssay: Assay, guessesRemaining: Int): Unit = {
//      println(
//        currentAssay.candidateWordAssaysSortedByScore.take(3)
//          .map(p => p._1 +" "+ p._2.summariseFor(assay.possibleWords.corpus)).mkString("\n"))
      val guess: Word = currentAssay.candidateWordAssaysSortedByScore.head._1
      val evidence = Evidence.evidenceFrom(guess, targetWord)
      val updatedAssay = currentAssay.updateWith(evidence)
      println(s"$evidence - ${updatedAssay.possibleWords.numPossibleWords} possible words left")
      if (!evidence.isSuccess && guessesRemaining>1) {
        takeAGuess(updatedAssay, guessesRemaining - 1)
      }
    }

    takeAGuess(assay, 6)
  }

  play(assay)

  play(assay)

}
