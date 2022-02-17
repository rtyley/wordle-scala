package com.madgag.wordle

case class WordGuessSum(wordId: WordId, guessSum: Int) extends Ordered[WordGuessSum] {
  override def compare(that: WordGuessSum): Int = guessSum.compareTo(that.guessSum)

  def addGuesses(x: Int) = copy(guessSum = guessSum + x)

  def word(using c: Corpus): Word = wordId.asWord

  def summary(using c: Corpus): String = {
    if (wordId>=0) {
      s"$word $guessSum avg=${guessSum.toFloat/c.initialCandidates.possibleWords.size}"
    } else "*nothing found yet*"

  }
}

object WordGuessSum {
  val TotalFailure = WordGuessSum(-1,1000000)
}