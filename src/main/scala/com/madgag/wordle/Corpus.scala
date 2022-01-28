package com.madgag.wordle

import com.google.common.io.{CharStreams, Resources}
import com.madgag.wordle.WordFeedback.feedbackFor
import com.madgag.wordle.Wordle.Word
import com.madgag.wordle.approaches.tartan.Candidates

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import scala.jdk.CollectionConverters.*
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.collection.immutable.{ArraySeq, BitSet, SortedSet}
import scala.util.Using
import scala.util.hashing.MurmurHash3

case class Corpus(commonWords: SortedSet[Word], uncommonWords: SortedSet[Word]) {
  val commonWordsOrdered: IndexedSeq[Word] = commonWords.toIndexedSeq.sorted
  val allWordsOrdered: IndexedSeq[Word]  = commonWordsOrdered ++ uncommonWords.toIndexedSeq

  val idForSetOfAllCommonWords: WordSet = PossibleWordSetStore.intern((0 until commonWords.size).toSet)

  val hash: Int = MurmurHash3.orderedHashing.hash(allWordsOrdered)

  val gridStorage: File = Path.of("/tmp", "wordle-scala-cache", s"$hash.data.gz").toFile

  // def humanReadableWordsFor(bitMap: BitSet): Set[Word] = bitMap.map(orderedCommonWords(_))

  val initialCandidates: Candidates = Candidates(
    possibleWords = SortedSet.from(0 until commonWords.size),
    discriminators = SortedSet.from(commonWords.size until allWordsOrdered.size)
  )

  def updateCandidatesWithEvidence(candidates: Candidates, evidenceWordId: Int, evidenceFeedback: WordFeedback): Candidates = {
    val gridEntryForEvidenceWord = grid(evidenceWordId)
    val (possibleWordsThatRemainPossible, possibleWordsThatBecameImpossible) =
      candidates.possibleWords.partition(gridEntryForEvidenceWord(_) == evidenceFeedback.underlying)

    // if the `Candidates` for possibleWordsThatRemainPossible is already available, return it, skip next part

    val updatedDiscriminators: SortedSet[Int] = (candidates.discriminators ++ possibleWordsThatBecameImpossible).filter { wordId =>
        val gridEntryForWord = grid(wordId)
        val firstFeedback = gridEntryForWord(possibleWordsThatRemainPossible.head)
        possibleWordsThatRemainPossible.tail.exists(gridEntryForWord(_) != firstFeedback)
      }

    Candidates(
      possibleWords = possibleWordsThatRemainPossible,
      discriminators = updatedDiscriminators
    )
  }


  lazy val grid: Array[Array[Byte]] = {
    println(gridStorage.getAbsolutePath)
    if (gridStorage.exists()) {
      Using.resource(new ObjectInputStream(new GZIPInputStream(new FileInputStream(gridStorage)))) { s =>
        s.readObject().asInstanceOf[Array[Array[Byte]]]
      }
    } else {
      val tmpFile = File.createTempFile("temp", "skunky")
      val gd = for {
        candidateWord <- allWordsOrdered.toArray
      } yield {
        println(s"candidateWord=$candidateWord")
        for (targetWord <- commonWordsOrdered) yield feedbackFor(candidateWord, targetWord).underlying
      }.toArray

      Using.resource(new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(tmpFile)))) { s =>
        s.writeObject(gd)
      }

      gridStorage.getParentFile.mkdirs()
      Files.move(tmpFile.toPath, gridStorage.toPath)
      gd
    }
  }
}

object Corpus {
  def fromAsteriskFormat(wordsWithAsterisk: Iterable[String]): Corpus = {
    val (popularWords, unpopularWords) = wordsWithAsterisk.partition(_.endsWith("*"))
    Corpus(SortedSet.from(popularWords.map(_.stripSuffix("*"))), SortedSet.from(unpopularWords))
  }

  def load(): Corpus = Corpus.fromAsteriskFormat(
    Resources.asCharSource(getClass.getResource("/wordle-five-letter-words.txt"), UTF_8).readLines().asScala //.take(4000)
  )
}