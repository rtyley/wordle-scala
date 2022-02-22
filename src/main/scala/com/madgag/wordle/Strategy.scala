package com.madgag.wordle

import com.madgag.wordle.Node.Choice
import com.madgag.wordle.approaches.tartan.{Candidates, FeedbackTable}
import com.madgag.wordle.evidence.{Evidence, WordFeedback}
import com.madgag.wordle.strategy_files.Line

import scala.collection.immutable.SortedMap

enum Node:
  case Choice(wordId: WordId, x: Map[WordFeedback, Node])
  case Success

object Strategy {
  val Win: (WordFeedback, Node) = WordFeedback.CompleteSuccess -> Node.Success

  case class ParsingState(rootChoice: Choice, path: List[(WordFeedback, WordId)]) {
    def accept(line: Line): ParsingState = {
      val updatedPath = path.take(line.guessIndexForHeadFeedback) ++ line.tailPairs
      ParsingState(ParsingState.add(rootChoice, updatedPath),updatedPath)
    }
  }
  object ParsingState {
    def add(choice: Choice, addingSuccessPath: List[(WordFeedback, WordId)]): Choice = {
      choice.copy(x = addingSuccessPath match {
        case (feedback, wordId) :: tail =>
          choice.x.updatedWith(feedback) { existingNodeOpt =>
            Some(add(existingNodeOpt.fold[Choice](Choice(wordId, Map.empty)) {
              case c: Choice => c
              case _ => throw new IllegalStateException(s"Can't update terminal success leaf $feedback on $choice !")
            }, tail))
          }
        case Nil => choice.x + Win
      })
    }
  }

  def treeFrom(lineStrs: Iterable[String])(using Corpus): Choice = {
    val lines = lineStrs.map(Line(_))
    val rootWordId = lines.head.rootWordIdOpt.get

    lines.foldLeft(ParsingState(Node.Choice(rootWordId, Map.empty), List.empty)) {
      case (parsingState, line) => parsingState.accept(line)
    }.rootChoice
  }

  def linesFrom(rootChoice: Node.Choice)(using Corpus): Iterable[Line] = {
    def tailPairsFrom(guessIndex: Int, choice: Choice): Seq[(Int, Seq[(WordFeedback, WordId)])] = {
      val feedbackAndNode: Seq[(WordFeedback, Node)] =
        SortedMap.from(choice.x)(WordFeedback.AlphabeticalOrdering).toSeq
      ???
    }

    for (((guessIndex, tailPairs), lineIndex) <- tailPairsFrom(0, rootChoice).zipWithIndex) yield Line(
      if (lineIndex==0) Right(rootChoice.wordId) else Left(guessIndex),
      tailPairs
    )
  }


  def fromPlayer(wordlePlayer: WordlePlayer, gameMode: GameMode)(using corpus: Corpus): Node.Choice = {
    val feedbackTable = FeedbackTable.obtainFor(gameMode)

    def choiceFromGameState(playerState: WordlePlayer.State, candidates: Candidates): Choice = {
      val word = playerState.move
      val wordId = word.id
      require(candidates.contains(wordId)) // otherwise the Player may be breaking the rules in Hard mode... note this only permits 'useful' moves
      Choice(wordId, for ((feedback, updatedCandidates) <- feedbackTable.possibleCandidateSetsIfCandidatePlayed(candidates, wordId)) yield {
        feedback -> (feedback match {
          case WordFeedback.CompleteSuccess => Node.Success
          case _ => choiceFromGameState(playerState.updateWith(Evidence(word, feedback)), updatedCandidates)
        })
      })
    }

    choiceFromGameState(wordlePlayer.start, corpus.initialCandidates)
  }
}


