package com.madgag.wordle

import com.madgag.wordle.Node.Choice
import com.madgag.wordle.approaches.tartan.{Candidates, FeedbackTable}
import com.madgag.wordle.strategy_files.Line


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

//    def forInitialLine(line: Line): ParsingState = {
//      require(line.headGuessIndex==0)
//
//      val path = line.headFeedback +: line.tailPairs.map(_._2)
//      val headWordId = line.headWordId.get
//      headWordId +: line.tailPairs.map(_._1)
//      val addingPath: List[(WordId, WordFeedback)] = ???
//      add(Choice(headWordId, Map.empty),
//    }
  }

  def treeFrom(lineStrs: Iterable[String])(using Corpus): Choice = {
    val lines = lineStrs.map(Line(_))

    val rootWordId = lines.head.rootWordIdOpt.get

    lines.foldLeft(ParsingState(Node.Choice(rootWordId, Map.empty), List.empty)) {
      case (parsingState, line) => parsingState.accept(line)
    }.rootChoice
  }

  def fromPlayer(wordlePlayer: WordlePlayer, gameMode: GameMode)(using corpus: Corpus): Node.Choice = {
    val feedbackTable = FeedbackTable.obtainFor(gameMode)

    def choiceFromGameState(playerState: WordlePlayer.State, candidates: Candidates): Choice = {
      val word = playerState.move
      val wordId = word.id
      Choice(wordId, for ((feedback, updatedCandidates) <-feedbackTable.possibleCandidateSetsIfCandidatePlayed(candidates, wordId)) yield {
        feedback -> (feedback match {
          case WordFeedback.CompleteSuccess => Node.Success
          case _ => choiceFromGameState(playerState.updateWith(Evidence(word, feedback)), updatedCandidates)
        })
      })
    }

    choiceFromGameState(wordlePlayer.start(gameMode), corpus.initialCandidates)
  }
}


