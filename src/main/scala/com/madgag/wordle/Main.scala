package com.madgag.wordle

import com.madgag.wordle.Wordle.*
import com.madgag.scala.collection.decorators.*

import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Paths}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global

@main def main() = {
  val wordleWords: Set[Word] =
    Files.readAllLines(Paths.get("/Users/roberto/wordle-five-letter-words.txt")).asScala.toSet

  val assay = Await.result(Assay.assayFor(wordleWords, wordleWords), Duration.Inf)

  // println(assay)
  println(assay.possibleWordsByFeedbackByCandidateWord.size)
  println(assay.possibleWordsByFeedbackByCandidateWord.head)
}
