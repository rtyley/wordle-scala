package com.madgag.wordle

case class SuccessValues(seq: IndexedSeq[Float]) {
  require(seq.zip(seq.tail).forall {
    case (current, later) => current >= later
  }, "Value should descend with each additional guess!")
  
  def apply(index: Int): Float = if (index < seq.length) seq(index) else 0
}

object SuccessValues {
  val Prototype: SuccessValues = SuccessValues(IndexedSeq(32,16,8,4,2,1))
}