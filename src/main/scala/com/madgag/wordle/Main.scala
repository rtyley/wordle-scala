package com.madgag.wordle

import com.madgag.wordle.Wordle.*
import com.madgag.scala.collection.decorators.*
import scala.jdk.CollectionConverters._
import java.nio.file.{Files, Paths}

@main def main() = {
  val wordleWords: Set[Word] =
    Files.readAllLines(Paths.get("/Users/roberto/wordle-five-letter-words.txt")).asScala.take(6000).toSet

  val assay = Assay.assayFor(wordleWords, wordleWords)

  // println(assay)
  println(assay.possibleWordsByFeedbackByCandidateWord.size)
  println(assay.possibleWordsByFeedbackByCandidateWord.head)
}
