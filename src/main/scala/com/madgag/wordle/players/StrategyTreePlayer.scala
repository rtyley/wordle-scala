package com.madgag.wordle.players

import com.madgag.wordle.*
import com.madgag.wordle.approaches.tartan.Candidates
import com.madgag.wordle.players.StrategyTreePlayer.Coverage

import scala.collection.immutable.SortedSet

object StrategyTreePlayer {
  case class Coverage(targetWordsResolved: Set[WordId] = Set.empty, wordsEncountered: Set[WordId] = Set.empty) {
    def +(other: Coverage): Coverage =
      Coverage(targetWordsResolved ++ other.targetWordsResolved, wordsEncountered ++ other.wordsEncountered )
  }

  def coverageOf(choice: Node.Choice): Coverage = {
    val choiceWordAsSet = Set(choice.wordId)
    choice.x.values.toSet.map {
      case c: Node.Choice => coverageOf(c)
      case Node.Success => Coverage(targetWordsResolved = choiceWordAsSet)
    }.reduce(_ + _) + Coverage(wordsEncountered = choiceWordAsSet)
  }
}

case class StrategyTreePlayer(rootChoice: Node.Choice) extends WordlePlayer {
  val treeCoverage: Coverage = StrategyTreePlayer.coverageOf(rootChoice)

  def start(gameMode: GameMode)(using corpus: Corpus): WordlePlayer.State = {
    require(corpus.commonWordsOrdered.indices.toSet == treeCoverage.targetWordsResolved)
    require(treeCoverage.wordsEncountered.subsetOf(corpus.allWordsOrdered.indices.map(_.asInstanceOf[WordId]).toSet))

    case class State(currentChoice: Node.Choice) extends WordlePlayer.State {
      lazy val move: Word = currentChoice.wordId.asWord

      def updateWith(evidence: Evidence): State = {
        require(evidence.word.id == currentChoice.wordId)
        State(currentChoice.x(evidence.wordFeedback).asInstanceOf[Node.Choice])
      }
    }

    State(rootChoice)
  }
}
