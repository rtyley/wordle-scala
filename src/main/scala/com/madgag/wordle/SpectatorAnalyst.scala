package com.madgag.wordle

import com.madgag.wordle.approaches.tartan.{Candidates, FeedbackTable}
import com.madgag.wordle.evidence.Evidence
import com.madgag.wordle.wordsets.*

import java.nio.file.{Files, Paths}
import scala.collection.immutable.SortedSet
import scala.jdk.CollectionConverters.*

case class SpectatorAnalyst(gameMode: GameMode)(using corpus: Corpus) {
  val feedbackTable: FeedbackTable = FeedbackTable.obtainFor(gameMode)

  def writeOutCsvFor(evidenceSoFar: Seq[Evidence], candidate: Word): Unit = {
    val candidateId = candidate.id
    val possWords = evidenceSoFar.foldLeft(corpus.initialCandidates.possibleWords) {
      case (possibleWords, evidence) =>
        println(s"Has candidate $candidate: "+possibleWords.contains(candidateId))
        val feedbackPartition = feedbackTable.partitionForCandidateGiven(possibleWords, evidence.guess.id)
        val wordSet = feedbackPartition.feedbackWithWords.toMap.apply(evidence.wordFeedback)
        println(s"wordSet ${wordSet.size} ${wordSet.contains(candidateId)}")
        wordSet
    }
    println("Yarg: "+possWords.size)
    println("Has candidate: "+possWords.contains(candidateId))
    val fileName = (evidenceSoFar.map(_.plainText) :+ candidate :+ "csv").mkString(".")
    println(possWords.map(_.asWord))

    println(feedbackTable.orderedCandidateOutlooksFor(Candidates(possWords, corpus.initialCandidates.discriminators)).take(10).map(_.t.asWord))
    val feedbackPartition = feedbackTable.partitionForCandidateGiven(possWords, candidateId)


    val path = corpus.storageDir.resolve(fileName)
    println(path)
    Files.write(path,
      feedbackPartition.asCSV.asJava
    )
  }


}


case class VisibleState(evidenceSoFar: Seq[Evidence], nextGuess: Option[Word] = None) {
  def javascriptCommands: String = {
    val indiciesToClear = (evidenceSoFar.size + nextGuess.size) until 6
    ((for ((evidence, rowIndex) <- evidenceSoFar.zipWithIndex) yield {
      s"setEvidence($rowIndex, '${evidence.guess}', '${evidence.wordFeedback.characters}');"
    }) ++ nextGuess.map(word => s"setWord(${evidenceSoFar.size}, '$word');") ++ indiciesToClear.map(index => s"setWord($index, '');")).mkString
  }
}
