package com.madgag.wordle

import com.google.common.io.Resources
import com.madgag.wordle.Corpus.{Full, fromAsteriskFormat, getClass}
import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.PlayAnalysis.forGameMode
import com.madgag.wordle.WordFeedback.fromChars
import org.scalatest.flatspec.AnyFlatSpec

import java.nio.charset.StandardCharsets.UTF_8

class StrategyTest extends AnyFlatSpec {
  val exampleLines: Seq[String]=
    Resources.asCharSource(getClass.getResource("/sonorous-chocolate/corpus-24-of-131-tree.txt"), UTF_8)
      .readLines().asScala.toSeq

  it should "parse a sonorous-chocolate strategy file" in {
    given Corpus = Full.reducedByAFactorOf(100)
    
    extension (feedbackChars: String)
      def guess(word:Word)(possibilities: (WordFeedback, Node)*): (WordFeedback, Node) =
        require(possibilities.map(_._1).groupBy(identity).values.forall(_.size == 1))
        fromChars(feedbackChars) -> Choice(word.id, Map(possibilities))
        
      def winWith(word:Word) = guess(word)(Win)
    
    Strategy.treeFrom(exampleLines) shouldBe Choice("laris".id, Map(
      "BBBBY".winWith("pesto"),
      "BBBGB".winWith("cubic"),
      "BBBYB".winWith("niche"),
      "BBGBY".winWith("purse"),
      "BBGYB".winWith("birch"),
      "BBYBB".winWith("drone"),
      "BBYYB".winWith("vigor"),
      "BBYYY".winWith("wrist"),
      "BGBBB".winWith("taboo"),
      "BGGBB".winWith("march"),
      "BYBBB".guess("aback")(Win, "YBBBB".winWith("human")),
      "BYBBY".guess("shady")(Win, "GBGBB".winWith("smack")),
      "BYYBB".winWith("tramp"),
      "BYYBY".winWith("grasp"),
      "BYYGY".winWith("stair"),
      "GGBBY".winWith("lasso"),
      "YBBBB".winWith("bulky"),
      "YGBBY".winWith("false"),
      "YYBBB".winWith("aptly"),
      "YYBBY".winWith("clash"),
      "YYYGB".winWith("frail"),
      "YYYYB".winWith("rival"),
    ))
  }
}