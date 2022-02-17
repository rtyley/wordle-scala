package com.madgag.wordle.wordsets.partition

import com.madgag.wordle.WordFeedback
import com.madgag.wordle.pooling
import com.madgag.wordle.wordsets.WordSet

object FeedbackPartition {
  class Pool(partitionPool: Partition.Pool) extends pooling.Pool[Map[WordFeedback, WordSet], FeedbackPartition] {
    override protected def create(id: Int, m: Map[WordFeedback, WordSet]): FeedbackPartition = {
      val p = partitionPool.intern(m.values.toSet)
      val feedbackByWordSet: Map[WordSet, WordFeedback] = m.map(_.swap)
      new FeedbackPartition(id, p.sets.map(feedbackByWordSet), p)
    }
  }
}

class FeedbackPartition private[partition](
  val id: Int,
  val feedbacks: Seq[WordFeedback],
  val partition: Partition
) extends pooling.Pooled(id) {
  require(feedbacks.size == partition.sets.size)

  def wordSetByFeedback: Seq[(WordFeedback, WordSet)] = feedbacks.zip(partition.sets)

  //   override val hashCode: Int = sets.hashCode() // we rely on the hashcode a lot for `Set`s, so compute once...!

  override def equals(that: Any): Boolean = that match {
    case p: FeedbackPartition if p.id == id => true
    case _ => super.equals(that)
  }
}