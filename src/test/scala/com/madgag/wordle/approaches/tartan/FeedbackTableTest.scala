package com.madgag.wordle.approaches.tartan

import com.madgag.wordle.*
import com.madgag.wordle.Game
import com.madgag.wordle.GameMode.Normal
import com.madgag.wordle.players.HackyMcHackFace
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.jdk.CollectionConverters._
import java.nio.file.{Files, Paths}

class FeedbackTableTest extends AnyFlatSpec with Matchers {

  given corpus: Corpus = Corpus.Full
  val feedbackTable = FeedbackTable.obtainFor(Normal)

  def writeOutCsvFor(word: Word): Unit = {
    val path = corpus.storageDir.resolve(s"$word.csv")
    println(path)
    Files.write(path,
      feedbackTable.partitionForCandidateGiven(
        corpus.initialCandidates.possibleWords, word.id
      ).asCSV.asJava
    )
  }

  it should "play Wordles" in {
    writeOutCsvFor("trace")
    writeOutCsvFor("roate")
    writeOutCsvFor("salet")
    writeOutCsvFor("jazzy")
  }
}
