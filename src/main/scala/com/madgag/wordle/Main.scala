package com.madgag.wordle

import com.madgag.wordle.Wordle.*
import com.madgag.scala.collection.decorators.*
import com.madgag.wordle.GameMode.*
import com.madgag.wordle.approaches.tartan.{FeedbackTable, Candidates}

import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Paths}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global
import scala.util.Random

@main def main() = {
  val corpus: Corpus = Corpus.load()

  val analysisForCorpusWithGameMode = FeedbackTable.obtainFor(corpus.withGameMode(Normal))

//  corpus.analyseGrid()

  playThing(corpus.initialCandidates)

//  println("TOP\n"+assay.candidateWordAssaysSortedByScore.take(5))
//
//  println("BOTTOM\n"+assay.candidateWordAssaysSortedByScore.takeRight(5))

  def playThing(candidates: Candidates): Unit = {
    val targetWord: Word = corpus.pickRandomTargetWord()
    //val bestCandidateId = corpus.bestCandidate(0, candidates, SuccessValues.Prototype)
    // println(corpus.allWordsOrdered(bestCandidateId))
  }

//  for (_ <- 1 to 20) {
//    play(assay)
//  }


}
