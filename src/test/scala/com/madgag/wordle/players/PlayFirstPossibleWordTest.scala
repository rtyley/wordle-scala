package com.madgag.wordle.players

import com.madgag.wordle.{Corpus, Game, Strategy, WordlePlayer}
import com.madgag.wordle.Corpus.Full
import com.madgag.wordle.GameMode.Normal
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PlayFirstPossibleWordTest extends AnyFlatSpec with Matchers with EitherValues {
  given corpus: Corpus = Full
  val player: WordlePlayer = PlayFirstPossibleWord.player

  it should "play Wordle..." in {
    println(Game("shake").playWith(player).value.text)
  }

  it should "play Wordle... surprisingly well sometimes" in {
    val gameConclusion = Game("caulk").playWith(player).value
    println(gameConclusion.text)
    gameConclusion.guessesTaken shouldBe 2 // wow!
  }

  it should "sadly fail on some words" in {
    val failedWords = corpus.commonWords.filter( targetWord =>
      !Game(targetWord).playWith(player).value.isSuccess
    )
    failedWords should not be empty // would be really surprising if this player always completed the wordle!
    println(s"Player fails to complete the Wordle with ${failedWords.size} words:\n${failedWords.mkString(", ")}.\n")
    val exampleOfFailingWord = failedWords.head
    println(Game(exampleOfFailingWord).playWith(player).value.text)
  }
}
