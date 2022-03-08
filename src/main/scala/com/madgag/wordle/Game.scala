package com.madgag.wordle

import cats.data.{NonEmptySeq, State, *}
import com.madgag.wordle.Game.WordNotPlayable
import com.madgag.wordle.Game.WordNotPlayable.{NotInWordList, *}
import com.madgag.wordle.GameMode.{Hard, Normal}
import com.madgag.wordle.evidence.Evidence
import com.madgag.wordle.evidence.Evidence.*
import com.madgag.wordle.wordsets.WordSet

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import cats.implicits.*

trait Playable {
  def play(word: Word): Either[WordNotPlayable, Game.State]
}

case class Game(targetWord: Word, gameMode: GameMode = Normal)(using val corpus: Corpus) extends Playable {

  def checkWordListFor(word: Word): Option[WordNotPlayable] = Option.when(!corpus.contains(word))(NotInWordList)

  def checkWordNotPlayable(word: Word, mostRecentEvidence: Evidence): Option[WordNotPlayable] =
    checkWordListFor(word).orElse(gameMode.checkConstraintsFor(word)(mostRecentEvidence))

  def play(word: Word): Either[WordNotPlayable, Game.State] =
    checkWordListFor(word).toLeft(Game.State(this, NonEmptySeq.one(word)))

  def playWith(wordlePlayer: WordlePlayer): Either[WordNotPlayable, Game.State] =
    play(wordlePlayer.start.move).flatMap(_.playWith(wordlePlayer))
}

object Game {
  enum WordNotPlayable:
    case NotInWordList
    case NotValidInHardMode(problem: Hard.PlayConstraint)

  trait Playable {
    def play(word: Word): Either[WordNotPlayable, State]
  }

  case class State(game: Game, playedWords: NonEmptySeq[Word]) extends Playable {

    val guessesTaken: Long = playedWords.size

    val evidenceSoFar: NonEmptySeq[Evidence] = playedWords.map(evidenceFrom(_, game.targetWord))
    val mostRecentEvidence: Evidence = evidenceSoFar.last

    val isSuccess: Boolean = mostRecentEvidence.isSuccess

    val shouldStopNow: Boolean = isSuccess || (guessesTaken == MaxGuesses)

    def play(word: Word): Either[WordNotPlayable, State] =
      game.checkWordNotPlayable(word, mostRecentEvidence).toLeft(State(game, playedWords :+ word))

    def playWith(wordlePlayer: WordlePlayer): Either[WordNotPlayable, State] =
      playWith(evidenceSoFar.foldLeft(wordlePlayer.start)(_ updateWith _))

    private def playWith(playerState: WordlePlayer.State): Either[WordNotPlayable, State] = {
      play(playerState.move).flatMap { newGameState =>
        if (newGameState.shouldStopNow) Right(newGameState) else
          newGameState.playWith(playerState.updateWith(newGameState.mostRecentEvidence))
      }
    }

    lazy val text: String = {
      for ((evidence, index) <- evidenceSoFar.toSeq.zipWithIndex) yield s"${index+1}. ${evidence.ansiColouredString}"
    }.mkString("\n")
  }


  def totalGuessSumFor(wordlePlayer: WordlePlayer, gameMode: GameMode)(using corpus: Corpus): Long = {
    corpus.commonWords.toSeq.map { targetWord =>
      val endGameState = Game(targetWord, gameMode).playWith(wordlePlayer).toOption.get
      require(endGameState.isSuccess)
      endGameState.guessesTaken
    }.sum
  }
}