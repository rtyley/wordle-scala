package com.madgag.wordle

import com.madgag.wordle.Node.Choice
import com.madgag.wordle.strategy_files.Line


enum Node:
  case Choice(wordId: WordId, x: Map[WordFeedback, Node])
  case Success


object Strategy {
  val Win: (WordFeedback, Node) = WordFeedback.CompleteSuccess -> Node.Success

//  case class ParsingState(path: Seq[WordFeedback], root: Node.Choice) {
//    def accept(line: Line): ParsingState = rootChoice.fold {
//      require(path.isEmpty)
//
//    } { old =>
//      old
//    }
//
//  }
  object ParsingState {
//    def add(choice: Choice, addingSuccessPath: List[(WordFeedback, WordId)]) = {
//      choice.copy(x = addingSuccessPath match {
//        case (feedback, wordId) :: Nil => choice.x.updated(feedback, Choice(wordId, Map(Win)))
//        case (feedback, wordId) :: tail =>
//          choice.x.updatedWith(feedback) { existingNodeOpt =>
//            add(existingNodeOpt.fold[Choice](Choice(wordId, Map.empty)) {
//              case c: Choice => c
//              case _ => throw new IllegalStateException(s"Can't update terminal success leaf $feedback on $choice !")
//            }, tail)
//          }
//
//      })
//    }

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

  def treeFrom(lines: Iterable[String])(using Corpus): Choice = {
//    lines.map(Line(_)).foldLeft(ParsingState(path = Seq.empty, None)) {
//      case (parsingState, line) => parsingState.accept(line)
//    }.currentChoiceOpt.get
    ???
  }
}


