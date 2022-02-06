package com.madgag.wordle

import com.madgag.wordle.approaches.tartan.Candidates

import scala.collection.immutable.SortedSet

sealed trait GameMode {
  def wordsRequiringEvaluationAsTargets(corpus: Corpus): IndexedSeq[Word]
}

object GameMode {

  object Normal extends GameMode {
    override def wordsRequiringEvaluationAsTargets(corpus: Corpus): IndexedSeq[Word] = corpus.commonWordsOrdered
  }

  object Hard extends GameMode {
    def wordsRequiringEvaluationAsTargets(corpus: Corpus) = corpus.allWordsOrdered
  }
}