package com.madgag.wordle

import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.PossibleWords.allWordsFrom
import com.madgag.wordle.approaches.tartan.AnalysisForCorpusWithGameMode
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExplorerTest extends AnyFlatSpec with Matchers {
  implicit val floatEquality: Equality[Float] = TolerantNumerics.tolerantFloatEquality(0.000001)

  behavior of "Explorer"

  val corpus = Corpus.fromAsteriskFormat(Seq("aback*", "defer*", "gully*", "angry"))
  val explorer = Explorer(AnalysisForCorpusWithGameMode.obtainFor(corpus.withGameMode(Normal)), SuccessValues(IndexedSeq(30, 22, 1)))

  it should "understand that we have a certain win on the second guess" in {
    explorer.expectedUtility(
      0,
      corpus.idFor("angry"),
      corpus.initialCandidates
    ) shouldBe 22
  }

  it should "understand that we stand a small chance of early victory, followed by random guessing" in {
    explorer.expectedUtility(
      0,
      corpus.idFor("aback"),
      corpus.initialCandidates
    ) should === ((30/3)+((2f/3)*((22/2)+(1f/2))))
  }

  it should "find the best candidate" in {
    corpus.allWordsOrdered(explorer.bestCandidate(
      0,
      corpus.initialCandidates
    )) shouldBe "angry"
  }
}