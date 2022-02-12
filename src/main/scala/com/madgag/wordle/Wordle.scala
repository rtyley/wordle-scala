package com.madgag.wordle

import com.madgag.scala.collection.decorators.*
import com.madgag.wordle.LetterFeedback.*
import com.madgag.wordle.WordFeedback.CompleteSuccess

import java.nio.file.{Files, Path, Paths}
import scala.collection.immutable.{BitSet, SortedMap}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

type Word = String
type WordId = Short
val MaxGuesses = 6

extension (word: Word)
  def id(using c: Corpus): WordId = c.idFor(word)

extension (wordId: WordId)
  def asWord(using c: Corpus): Word = c.allWordsOrdered(wordId)

object Wordle {
  val WordLength = 5
  val WordIndices: Seq[Int] = 0 until WordLength

  type Letter = Char
}


