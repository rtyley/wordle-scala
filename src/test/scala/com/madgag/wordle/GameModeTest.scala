package com.madgag.wordle

import com.madgag.wordle.Game.WordNotPlayable
import com.madgag.wordle.Game.WordNotPlayable.NotValidInHardMode
import com.madgag.wordle.GameMode.Hard.PlayConstraint.*
import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.PlayAnalysis.forGameMode
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.matchers.should.Matchers

class GameModeTest extends AnyFlatSpec with Matchers with OptionValues {
  it should "enforce a required correct letter just like official Wordle" in {
    import GameMode.Hard.checkConstraintsFor

    val gottaStartWithAnO = Evidence("offal", WordFeedback("🟩⬜⬜⬜⬜"))

    checkConstraintsFor("bogus")(gottaStartWithAnO).value shouldBe NotValidInHardMode(NthLetterMustBe(0, 'o'))
    checkConstraintsFor("oasis")(gottaStartWithAnO) shouldBe None
  }

  it should "enforce a required letter just like official Wordle" in {
    import GameMode.Hard.checkConstraintsFor

    val gottaStartWithAnOAndHaveAT = Evidence("octal", WordFeedback("🟩⬜🟨⬜⬜"))

    checkConstraintsFor("oasis")(gottaStartWithAnOAndHaveAT).value shouldBe NotValidInHardMode(MustContain('t'))
    checkConstraintsFor("total")(gottaStartWithAnOAndHaveAT).value shouldBe NotValidInHardMode(NthLetterMustBe(0, 'o'))

    // There are 2 constraint violations here, Wordle happens to pick the stricter `NthLetterMustBe` one
    // It's not really essential that we fully reproduce that choice of error message, could relax this assertion:
    checkConstraintsFor("bogus")(gottaStartWithAnOAndHaveAT).value shouldBe NotValidInHardMode(NthLetterMustBe(0, 'o'))
  }

}
