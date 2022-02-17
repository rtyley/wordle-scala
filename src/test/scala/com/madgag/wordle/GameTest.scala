package com.madgag.wordle

import com.madgag.wordle.Corpus.fromAsteriskFormat
import com.madgag.wordle.GameMode.Normal
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, OptionValues}

class GameTest extends AnyFlatSpec with Matchers with EitherValues {
  it should "analyse Roberto's game of Wordle for 'robin'" in {
    given c: Corpus = Corpus.Full

    val game = Game("robin", Normal)

    val after1stGuess = game.start.play("salet").value
    // println(s"After 1st guess ${after1stGuess.possibleWords.size} possible words")
    val after2ndGuess = after1stGuess.play("boing").value

    println(after2ndGuess.text)
  }
}
