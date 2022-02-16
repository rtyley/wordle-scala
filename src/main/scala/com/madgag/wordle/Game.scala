package com.madgag.wordle

import com.madgag.wordle.Evidence.evidenceFrom
import com.madgag.wordle.approaches.tartan.*
import com.madgag.wordle.wordsets.WordSet

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

case class Game(targetWord: Word, gameMode: GameMode)(using val corpus: Corpus) {
  val feedbackTable: FeedbackTable = FeedbackTable.obtainFor(gameMode)

  val targetWordId: WordId = targetWord.id

  private val initialPermittedWords: WordSet = WordSet.fromKnownDistinct(corpus.initialCandidates.allWords)

  val start: State = State(Seq.empty, initialPermittedWords)

  case class State(playedWords: Seq[Word], permittedWords: WordSet) {

    val guessesTaken: Int = playedWords.size

    def play(word: Word): Either[String, State] = {
      val wordId = word.id
      if (wordId < 0) Left(s"'$word' is not in corpus (${corpus.id})") else {
        if (!permittedWords.contains(wordId)) Left("'$word' is not permitted now") else {
          val wordFeedback = WordFeedback.feedbackFor(word, targetWord)

          Right(State(playedWords :+ word, feedbackTable.updatedPermittedWordsGiven(permittedWords, wordId, wordFeedback)))
        }
      }
    }

    def playWith(wordlePlayer: WordlePlayer): State = {
      val playerState = evidenceSoFar.foldLeft(wordlePlayer.start) {
        case (player, evidence) => player.updateWith(evidence)
      }

      playWith(playerState)
    }

    @tailrec
    private def playWith(playerState: WordlePlayer.State): State = {
      val updatedGameState: State = play(playerState.move).toOption.get
      val resultingEvidence = updatedGameState.evidenceSoFar.last
      if (updatedGameState.shouldStopNow) updatedGameState else
        updatedGameState.playWith(playerState.updateWith(resultingEvidence))
    }

    val isSuccess: Boolean = evidenceSoFar.lastOption.exists(_.isSuccess)

    val shouldStopNow: Boolean = isSuccess || (guessesTaken == MaxGuesses)

    def canPlay(word: Word): Boolean = permittedWords.contains(word.id)

    lazy val evidenceSoFar: Seq[Evidence] = playedWords.map(evidenceFrom(_, targetWord))

    lazy val text: String = {
      for ((evidence, index) <- evidenceSoFar.zipWithIndex) yield s"${index+1}. ${evidence.ansiColouredString}"
    }.mkString("\n")
  }

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