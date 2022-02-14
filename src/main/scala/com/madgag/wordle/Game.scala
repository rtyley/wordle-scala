package com.madgag.wordle

import com.madgag.wordle.Evidence.evidenceFrom
import com.madgag.wordle.approaches.tartan.*
import scala.collection.immutable.SortedSet

case class Game(targetWord: Word, gameMode: GameMode)(using val corpus: Corpus) {
  val feedbackTable: FeedbackTable = FeedbackTable.obtainFor(gameMode)

  val targetWordId: WordId = targetWord.id

  val start: GameState = GameState(this, Seq.empty, corpus.initialCandidates)
}

object Game {
  def totalGuessSumFor(wordlePlayer: WordlePlayer, gameMode: GameMode)(using corpus: Corpus): Int = {
    corpus.commonWords.toSeq.map { targetWord =>
      val endGameState = Game(targetWord, gameMode).start.playWith(wordlePlayer)
      require(endGameState.isSuccess)
      endGameState.guessesTaken
    }.sum
  }
}

case class GameState(game: Game, playedWords: Seq[Word], candidates: Candidates) {
  given corpus: Corpus = game.corpus

  val guessesTaken: Int = playedWords.size

  def play(word: Word): Either[String, GameState] = {
    val wordId = word.id
    if (wordId < 0) Left(s"'$word' is not in corpus (${corpus.id})") else {
      if (!candidates.contains(wordId)) Left("'$word' either prohibited, or does not discriminate solutions") else {
        val wordFeedback = WordFeedback.feedbackFor(word, game.targetWord)

        val updatedCandidates = game.feedbackTable.possibleCandidateSetsIfCandidatePlayed(candidates, wordId)(wordFeedback)
        Right(GameState(game, playedWords :+ word, updatedCandidates))
      }
    }
  }

  def playWith(wordlePlayer: WordlePlayer): GameState = {
    val playerState = evidenceSoFar.foldLeft(wordlePlayer.start(game.gameMode)) {
      case (player, evidence) => player.updateWith(evidence)
    }

    playWith(playerState)
  }

  private def playWith(playerState: WordlePlayerState): GameState = {
    val updatedGameState: GameState = play(playerState.move).toOption.get
    val resultingEvidence = updatedGameState.evidenceSoFar.last
    println(s"${updatedGameState.guessesTaken}. ${resultingEvidence.ansiColouredString}")
    if (updatedGameState.shouldStopNow) updatedGameState else
      updatedGameState.playWith(playerState.updateWith(resultingEvidence))
  }

  val isSuccess: Boolean = evidenceSoFar.lastOption.exists(_.isSuccess)

  val shouldStopNow: Boolean = isSuccess || (guessesTaken == MaxGuesses)

  def canPlay(word: Word): Boolean = candidates.contains(game.corpus.idFor(word))

  lazy val evidenceSoFar: Seq[Evidence] = playedWords.map(evidenceFrom(_, game.targetWord))

  lazy val possibleWords: SortedSet[Word] = candidates.possibleWords.map(_.asWord)

  def candidatesPartitionFor(word: Word) = game.feedbackTable.possibleCandidateSetsIfCandidatePlayed(candidates, word.id)
}