package com.madgag.wordle.players

import com.madgag.scala.collection.decorators.*
import com.google.common.io.Resources
import com.madgag.wordle.*
import com.madgag.wordle.Corpus.{Full, getClass}
import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.StrategyExample.given_Corpus
import com.madgag.wordle.approaches.tartan.FeedbackTable
import com.madgag.wordle.wordsets.partition.FeedbackPartition
import com.madgag.wordle.{Corpus, Game, StrategyExample}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*
import java.nio.charset.StandardCharsets.UTF_8

class StrategyTreePlayerTest extends AnyFlatSpec with Matchers {
  it should "play Wordles" in {
    import StrategyExample.*
    given Corpus = StrategyExample.given_Corpus

    val player = StrategyTreePlayer(exampleStrategyTree).playing

    Game.totalGuessSumFor(player, Normal) shouldBe 50
  }

  it should "play Wordles that we get wrong" in {
    given Corpus = Full.reducedByAFactorOf(90)
    val rootChoice = Strategy.treeFrom(
      Resources.asCharSource(getClass.getResource("/sonorous-chocolate/tree.corpus-26-of-145-words__F1978AB0.Normal.txt"), UTF_8)
        .readLines().asScala.toSeq)

    val player = StrategyTreePlayer(rootChoice).playing

    Game.totalGuessSumFor(player, Normal) shouldBe 58
  }


  it should "work out when agora would be useful" in {
    given corpus: Corpus = Full
    val feedbackTable = FeedbackTable.obtainFor(GameMode.Normal)
    val agoraId: WordId = "agora".id

    println("I am here")
    val pairs = for {
      startWordId <- corpus.initialCandidates.possibleWords.toSeq
      (feedback, wordSet) <- feedbackTable.partitionForCandidateGiven(corpus.initialCandidates.possibleWords - agoraId, startWordId).wordSetByFeedback
      agoraPartition = feedbackTable.partitionForCandidateGiven(wordSet, agoraId).partition
      if feedback != WordFeedback.CompletelyIncorrect && feedback != WordFeedback.CompleteSuccess && wordSet.size < 40 && wordSet.size > 8 && agoraPartition.sets.forall(_.size <3) && agoraPartition.sets.count(_.size ==1) > wordSet.size*3/4
    } yield {
      val wordsToTry = corpus.initialCandidates.allWords
      val wordsWorseThanAgora = wordsToTry.count { secondWordId =>
        val partition = feedbackTable.partitionForCandidateGiven(wordSet, secondWordId).partition
          partition.evennessScore > agoraPartition.evennessScore
        }
      val wordsBetterOrEqualToAgora = wordsToTry.size - wordsWorseThanAgora
      val score = wordsWorseThanAgora.toFloat / (1 + wordsBetterOrEqualToAgora)
      // println(agoraIndex)
      (Evidence(startWordId.asWord, feedback), score)
    }

    for ((initialEvidence, score) <- pairs.sortBy(_._2).takeRight(100)) {
      println(s"\n$initialEvidence : $score\n")
      val possibleWordsAfterFirstGuess = feedbackTable.partitionForCandidateGiven(corpus.initialCandidates.possibleWords, initialEvidence.word.id).wordSetByFeedback.toMap.apply(initialEvidence.wordFeedback)
      println(possibleWordsAfterFirstGuess.map(_.asWord))
      val agoraPartition = feedbackTable.partitionForCandidateGiven(possibleWordsAfterFirstGuess, agoraId)
      println(agoraPartition.wordSetByFeedback.toMap.mapV(_.map(_.asWord)))
    }
  }
}
