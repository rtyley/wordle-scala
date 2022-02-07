package com.madgag.wordle.wordsets

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShortArrayWordSetTest extends AnyFlatSpec with Matchers {
  it should "be ok to insert a single element" in {
    ShortArrayWordSet(1,2,3,4).incl(5).toSeq shouldBe Seq(1,2,3,4,5)
    ShortArrayWordSet(1,2,4,5).incl(3).toSeq shouldBe Seq(1,2,3,4,5)
    ShortArrayWordSet(2,3,4,5).incl(1).toSeq shouldBe Seq(1,2,3,4,5)
  }

  it should "be ok to remove a single element" in {
    ShortArrayWordSet(1,2,3,4,5).excl(5).toSeq shouldBe Seq(1,2,3,4)
    ShortArrayWordSet(1,2,3,4,5).excl(3).toSeq shouldBe Seq(1,2,4,5)
    ShortArrayWordSet(1,2,3,4,5).excl(1).toSeq shouldBe Seq(2,3,4,5)
  }

  it should "ideally remove duplicates" in {
    ShortArrayWordSet.fromSpecific(Seq(1,2,2,3)).toSeq shouldBe Seq(1,2,3)
  }
}
