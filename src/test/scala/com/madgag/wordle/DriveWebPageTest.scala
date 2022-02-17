package com.madgag.wordle

import com.madgag.wordle.Evidence.evidenceFrom
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DriveWebPageTest extends AnyFlatSpec with Matchers {
  it should "generate commands reflecting data" in {
    println(DriveWebPage.javascriptCommandsFor(Seq(
      evidenceFrom("money", "boing"),
      evidenceFrom("tango", "boing"),
    ),Some("mango")))
  }

}
