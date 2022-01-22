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
  val corpus: Corpus = Corpus(
    Files.readAllLines(Paths.get("/Users/roberto/wordle-five-letter-words.txt")).asScala.toSet
  )



  val assay = Await.result(Assay.assayFor(PossibleWords.allWordsFrom(corpus)), Duration.Inf)

  // println(assay)
  println(assay.possibleWordsByFeedbackByCandidateWord.size)
  println(assay.possibleWordsByFeedbackByCandidateWord.head)
  println("TOP\n"+assay.candidateWordAssaysSortedByScore.take(5))

  println("BOTTOM\n"+assay.candidateWordAssaysSortedByScore.takeRight(5))

  def play(assay: Assay): Unit = {
    val targetWord: Word = Random.shuffle(assay.possibleWords.corpus.orderedWords).head
    println(s"Target is $targetWord")
    def takeAGuess(currentAssay: Assay, guessesRemaining: Int): Unit = {
      val guess: Word = currentAssay.candidateWordAssaysSortedByScore.head._1
      val evidence = Evidence.evidenceFrom(guess, targetWord)
      println(evidence)
      if (!evidence.isSuccess && guessesRemaining>0) takeAGuess(currentAssay.updateWith(evidence), guessesRemaining-1 )
    }

    takeAGuess(assay, 6)
  }

  play(assay)

  play(assay)

}
