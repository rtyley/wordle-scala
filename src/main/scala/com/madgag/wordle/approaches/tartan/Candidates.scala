package com.madgag.wordle.approaches.tartan

import com.madgag.wordle.*
import com.madgag.wordle.wordsets.WordSet

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, LongAdder}
import scala.collection.immutable.SortedSet
import scala.math.Ordering

object Candidates {
  val creationCounter = new AtomicInteger()
  private val all: scala.collection.mutable.Set[Candidates] = scala.collection.mutable.Set()

  private val candidatesByTuple: java.util.concurrent.ConcurrentMap[(WordSet,WordSet),Candidates] =
    new java.util.concurrent.ConcurrentHashMap()

  def stored: Int = candidatesByTuple.size()

  def apply(possibleWords: WordSet, discriminators: WordSet): Candidates = {
    candidatesByTuple.computeIfAbsent((possibleWords, discriminators), { _ =>
      new Candidates(creationCounter.getAndIncrement(), possibleWords, discriminators)
    })
  }
}

class Candidates private(
  val id: Int,
  val possibleWords: WordSet,
  val discriminators: WordSet
) {
  def contains(word: WordId) = possibleWords.contains(word) || discriminators.contains(word)

  def allWords: Iterable[WordId] = possibleWords.view ++ discriminators

  override def hashCode(): Int = id

  override def equals(that: Any): Boolean = that match {
    case c: Candidates if c.id == id => true
    case _ => super.equals(that)
  }
}
