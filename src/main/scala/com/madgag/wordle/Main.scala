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
  given corpus: Corpus = Corpus.Full


}
