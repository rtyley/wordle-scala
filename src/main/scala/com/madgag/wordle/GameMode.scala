package com.madgag.wordle

import com.madgag.wordle.Wordle.Word

enum GameMode(val wordsRequiringEvaluationAsTargets: Corpus => IndexedSeq[Word]) {
  case Normal extends GameMode(
    wordsRequiringEvaluationAsTargets = _.commonWordsOrdered
  )
  case Hard extends GameMode(
    wordsRequiringEvaluationAsTargets = _.allWordsOrdered
  )
}