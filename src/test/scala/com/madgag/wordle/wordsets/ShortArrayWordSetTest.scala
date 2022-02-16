package com.madgag.wordle.wordsets

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedSet

class ShortArrayWordSetTest extends AnyFlatSpec with Matchers {
  it should "be ok to insert a single element" in {
    WordSet(1,2,3,4).incl(5).toSeq shouldBe Seq(1,2,3,4,5)
    WordSet(1,2,4,5).incl(3).toSeq shouldBe Seq(1,2,3,4,5)
    WordSet(2,3,4,5).incl(1).toSeq shouldBe Seq(1,2,3,4,5)
  }

  it should "be ok to remove a single element" in {
    WordSet(1,2,3,4,5).excl(5).toSeq shouldBe Seq(1,2,3,4)
    WordSet(1,2,3,4,5).excl(3).toSeq shouldBe Seq(1,2,4,5)
    WordSet(1,2,3,4,5).excl(1).toSeq shouldBe Seq(2,3,4,5)
  }

  it should "be ok to remove several elements" in {
    WordSet(1,2,3,4,5).removedAll(WordSet(2,3,4)).toSeq shouldBe Seq(1,5)
    WordSet(1,2,3,4,5).removedAll(WordSet(1,5)).toSeq shouldBe Seq(2,3,4)
    WordSet(1,2,3,4,5).removedAll(WordSet(1,3,5)).toSeq shouldBe Seq(2,4)
  }

  it should "ideally remove duplicates" in {
    WordSet.fromSpecific(Seq(1,2,2,3)).toSeq shouldBe Seq(1,2,3)
  }

  it should "have a stable hashcode" in {
    WordSet.fromSpecific(Seq(1,2,3)).hashCode shouldEqual WordSet.fromSpecific(Seq(3,2,1)).hashCode
  }
}
