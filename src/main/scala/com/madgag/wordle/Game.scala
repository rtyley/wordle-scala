package com.madgag.wordle

import com.madgag.wordle.Evidence.evidenceFrom
import com.madgag.wordle.approaches.tartan.*
import scala.collection.immutable.SortedSet

case class Game(targetWord: Word, corpusWithGameMode: CorpusWithGameMode) {
  val corpus = corpusWithGameMode.corpus

  val analysis = AnalysisForCorpusWithGameMode.obtainFor(corpusWithGameMode)

  val targetWordId = corpusWithGameMode.corpus.idFor(targetWord)

  val start = GameState(this, Seq.empty, corpus.initialCandidates)
}

case class GameState(game: Game, playedWords: Seq[Word], candidates: Candidates) {
  def play(word: Word): Either[String, GameState] = {
    val wordId = game.corpus.idFor(word)
    if (wordId < 0) Left(s"'$word' is not in corpus (${game.corpus.id})") else {
      if (!candidates.contains(wordId)) Left("'$word' either prohibited, or does not discriminate solutions") else {
        val wordFeedback = WordFeedback.feedbackFor(word, game.targetWord)

        val updatedCandidates = game.analysis.possibleCandidateSetsIfCandidatePlayed(candidates, wordId)(wordFeedback)
        Right(GameState(game, playedWords :+ word, updatedCandidates))
      }
    }
  }
  
  def canPlay(word: Word): Boolean = candidates.contains(game.corpus.idFor(word))
  
  lazy val evidence: Seq[Evidence] = playedWords.map(evidenceFrom(_, game.targetWord))
  
  lazy val possibleWords: SortedSet[Word] = candidates.possibleWords.map(game.corpus.allWordsOrdered(_))
}