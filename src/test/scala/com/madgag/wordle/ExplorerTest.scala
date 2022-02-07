package com.madgag.wordle

import com.madgag.wordle.GameMode.*
import com.madgag.wordle.approaches.tartan.AnalysisForCorpusWithGameMode
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExplorerTest extends AnyFlatSpec with Matchers with EitherValues {
  implicit val floatEquality: Equality[Float] = TolerantNumerics.tolerantFloatEquality(0.000001)

  behavior of "Explorer"

  val corpus = Corpus.fromAsteriskFormat(Seq("aback*", "defer*", "gully*", "angry"))
  val V: SuccessValues = SuccessValues(IndexedSeq(30, 22, 1))
  val normalExplorer = Explorer(AnalysisForCorpusWithGameMode.obtainFor(corpus.withGameMode(Normal)), V)

  it should "understand that we have a certain win on the second guess" in {
    normalExplorer.expectedUtility(
      0,
      corpus.idFor("angry"),
      corpus.initialCandidates
    ) shouldBe 22
  }

  it should "understand that we stand a small chance of early victory, followed by random guessing" in {
    normalExplorer.expectedUtility(
      0,
      corpus.idFor("aback"),
      corpus.initialCandidates
    ) should === ((V(0)/3)+((2f/3)*((V(1)/2)+(V(2)/2))))
  }

  it should "find the best candidate" in {
    corpus.allWordsOrdered(normalExplorer.bestCandidate(
      0,
      corpus.initialCandidates
    )) shouldBe "angry"
  }

  it should "do hard mode" in {
    val c = Corpus.fromAsteriskFormat(Seq("eight*", "light*", "might*", "sight*", "tight*", "smelt"))
    val hardExplorer = Explorer(AnalysisForCorpusWithGameMode.obtainFor(c.withGameMode(Hard)), V)
    hardExplorer.expectedUtility(
      0,
      c.idFor("eight"),
      c.initialCandidates
    ) should === ((V(0)/5)+((4f/5)*((V(1)/4)+((3f/4)*V(2)/3))))

    hardExplorer.expectedUtility(
      0,
      c.idFor("smelt"),
      c.initialCandidates
    ) should === (V(1))

    c.allWordsOrdered(hardExplorer.bestCandidate(
      0,
      c.initialCandidates
    )) shouldBe "smelt"
  }

  it should "play hard mode" in {
    val corpusWithGameMode = Corpus.load().withGameMode(Hard)
    println(s"Loaded ${corpusWithGameMode.corpus.id}")
    val game = Game(targetWord = "fight", corpusWithGameMode)
    val gameState = game.start
    println(s"We've started da game!")
    val g1state = gameState.play("might").value
    g1state.evidence.last.wordFeedback.emojis shouldBe "⬜🟩🟩🟩🟩"
    g1state.candidates.discriminators shouldBe empty
    g1state.canPlay("light") shouldBe true
    g1state.canPlay("forge") shouldBe false
    println(s"g1state.possibleWords = ${g1state.possibleWords}")
    val hardExplorer = Explorer(game.analysis, V)
//    val bestWordId = hardExplorer.bestCandidate(0, corpusWithGameMode.corpus.initialCandidates)
//    println(bestWordId)
//    bestWordId
  }

  it should "find the best candidate for a moderately large corpus in HARD mode" in {
    val c = Corpus.load().reduceByAFactorOf(22)
    val explorer = Explorer(AnalysisForCorpusWithGameMode.obtainFor(c.withGameMode(Hard)), SuccessValues(IndexedSeq(30,20,10,5,2,1)))

    println(c.allWordsOrdered(explorer.bestCandidate(0, c.initialCandidates)))
  }

  it should "find the best candidate for a moderately large corpus in Normal mode" in {
    val c = Corpus.load().reduceByAFactorOf(22)
    val explorer = Explorer(AnalysisForCorpusWithGameMode.obtainFor(c.withGameMode(Normal)), SuccessValues(IndexedSeq(30,20,10)))

    println(c.allWordsOrdered(explorer.bestCandidate(0, c.initialCandidates)))
  }


}