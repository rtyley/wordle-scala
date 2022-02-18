package com.madgag.wordle

import com.madgag.wordle.Evidence.*
import com.madgag.wordle.Game.WordNotPlayable
import com.madgag.wordle.GameMode.{Hard, Normal}
import com.madgag.wordle.Game.WordNotPlayable.*
import com.madgag.wordle.wordsets.WordSet

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

case class Game(targetWord: Word, gameMode: GameMode = Normal)(using val corpus: Corpus) {

  val start: State = State(Seq.empty)

  def playWith(wordlePlayer: WordlePlayer) = start.playWith(wordlePlayer)

  case class State(playedWords: Seq[Word]) {

    val guessesTaken: Int = playedWords.size

    val evidenceSoFar: Seq[Evidence] = playedWords.map(evidenceFrom(_, targetWord))
    val mostRecentEvidence: Option[Evidence] = evidenceSoFar.lastOption

    val isSuccess: Boolean = mostRecentEvidence.exists(_.isSuccess)

    val shouldStopNow: Boolean = isSuccess || (guessesTaken == MaxGuesses)

    def checkWordNotPlayable(word: Word): Option[WordNotPlayable] =
      if (!corpus.contains(word)) Some(NotInWordList) else mostRecentEvidence.flatMap(gameMode.checkConstraintsFor(word))

    def play(word: Word): Either[WordNotPlayable, State] = checkWordNotPlayable(word).toLeft(State(playedWords :+ word))

    def playWith(wordlePlayer: WordlePlayer): Either[WordNotPlayable, State] = playWith(evidenceSoFar.foldLeft(wordlePlayer.start) {
      case (player, evidence) => player.updateWith(evidence)
    })

    private def playWith(playerState: WordlePlayer.State): Either[WordNotPlayable, State] = {
      play(playerState.move).flatMap { newGameState =>
        if (newGameState.shouldStopNow) Right(newGameState) else
          newGameState.playWith(playerState.updateWith(newGameState.mostRecentEvidence.get))
      }
    }

    lazy val text: String = {
      for ((evidence, index) <- evidenceSoFar.zipWithIndex) yield s"${index+1}. ${evidence.ansiColouredString}"
    }.mkString("\n")
  }

}

object Game {
  enum WordNotPlayable:
    case NotInWordList
    case NotValidInHardMode(problem: Hard.PlayConstraint)

  def totalGuessSumFor(wordlePlayer: WordlePlayer, gameMode: GameMode)(using corpus: Corpus): Int = {
    corpus.commonWords.toSeq.map { targetWord =>
      val endGameState = Game(targetWord, gameMode).start.playWith(wordlePlayer).toOption.get
      require(endGameState.isSuccess)
      endGameState.guessesTaken
    }.sum
  }
}