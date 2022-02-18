package com.madgag.wordle

import com.madgag.wordle.Game.WordNotPlayable
import com.madgag.wordle.Game.WordNotPlayable.NotValidInHardMode
import com.madgag.wordle.GameMode.Hard.PlayConstraint.NthLetterMustBe
import com.madgag.wordle.Wordle.WordIndices
import com.madgag.wordle.approaches.tartan.Candidates

import scala.collection.immutable.SortedSet

sealed trait GameMode {
  def wordsRequiringEvaluationAsTargets(corpus: Corpus): IndexedSeq[Word]
  def checkConstraintsFor(word: Word)(prior: Evidence): Option[WordNotPlayable]
}

object GameMode {

  object Normal extends GameMode {
    def wordsRequiringEvaluationAsTargets(corpus: Corpus): IndexedSeq[Word] = corpus.commonWordsOrdered
    def checkConstraintsFor(word: Word)(prior: Evidence): Option[WordNotPlayable] = None
  }

  object Hard extends GameMode {
    def wordsRequiringEvaluationAsTargets(corpus: Corpus) = corpus.allWordsOrdered
    def checkConstraintsFor(word: Word)(prior: Evidence): Option[WordNotPlayable] = {
      val summary = prior.summary

      (summary.correctLettersByIndex.collectFirst {
        case (index, correctLetter) if word(index) != correctLetter => NthLetterMustBe(index, correctLetter)
      } orElse {
        val letterQuantitiesUsedInNonCorrectIndices = summary.indicesWhichAreNotCorrect.map(word).letterFrequency
        summary.misplacedLetters.collectFirst {
          case (letter, knownQuantity) if letterQuantitiesUsedInNonCorrectIndices(letter) < knownQuantity =>
            PlayConstraint.MustContain(letter)
        }
      }).map(NotValidInHardMode(_))
    }

    enum PlayConstraint:
      case MustContain(letter: Letter) // "Must contain 'E'" - report only mentions first missing letter
      case NthLetterMustBe(index: Int, letter: Letter) // "4th letter must be 'G'" - the correct letter is now fixed in place
  }
}