package com.madgag.wordle.wordsets.partition

import com.madgag.wordle.{WordFeedback, pooling}
import com.madgag.wordle.approaches.tartan.Candidates
import com.madgag.wordle.pooling.{Pool, Pooled}
import com.madgag.wordle.wordsets.WordSet

import java.util.concurrent.atomic.AtomicInteger

object Partition {
  class Pool extends pooling.Pool[Set[WordSet], Partition] {
    override def create(id: Int, sets: Set[WordSet]): Partition =
      Partition(id, sets.toSeq.sortBy(_.head))
  }
}

class Partition private[partition](
  val id: Int,
  val sets: Seq[WordSet]
) extends Pooled(id) {
  val evennessScore: Int = sets.map(s => s.size * s.size).sum
  
//   override val hashCode: Int = sets.hashCode() // we rely on the hashcode a lot for `Set`s, so compute once...!

  override def equals(that: Any): Boolean = that match {
    case p: Partition if p.id == id => true
    case _ => super.equals(that)
  }
}