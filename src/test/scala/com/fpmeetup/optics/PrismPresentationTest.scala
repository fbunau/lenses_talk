package com.fpmeetup.optics

import monocle.Prism
import org.scalatest.{FreeSpec, Matchers}

class PrismPresentationTest extends FreeSpec with Matchers {

  // see here a more complicated example: https://github.com/julien-truffaut/Monocle/blob/master/example/src/test/scala/monocle/JsonExample.scala

  sealed trait Day
  case object Monday extends Day
  case object Tuesday extends Day
  case object Wednesday extends Day

  private val mondayPrism = Prism[Day, Unit] {
    case Monday => Some(())
    case _ => None
  } (_ => Monday)

  "A case of the mondays" in {
    mondayPrism.getOption(Monday) shouldBe Some(())
  }

  "A case of the tuesdays" in {
    mondayPrism.getOption(Tuesday) shouldBe None
  }

  sealed trait Json
  case class JNumber(v: Double) extends Json
  case class JString(s: String) extends Json

  private val doubleJsonPrism = Prism[Json, Double] {
    case JNumber(v) => Some(v)
    case _ => None
  } (v => JNumber(v))

  "Usage of prism - defined" in {
    doubleJsonPrism.modify(_ + 1)(JNumber(2.0)) shouldBe JNumber(3.0)
  }

  "Usage of prism - undefined" in {
    doubleJsonPrism.modify(_ + 1)(JString("test")) shouldBe JString("test")
  }

  private val doubleIntPrism = Prism[Double, Int] {
    case d: Double if d % 1 == 0 => Some(d.toInt)
    case _ => None
  } (_.toDouble)

  "Usage of double int prism" in {
    doubleIntPrism.getOption(3.4) shouldBe None
    doubleIntPrism.getOption(3) shouldBe Some(3)
  }

  "Composing prisms" in {
    val prism = doubleJsonPrism composePrism doubleIntPrism

    prism.modify(_ + 1)(JNumber(3.4)) shouldBe JNumber(3.4)
    prism.modify(_ + 1)(JNumber(3.0)) shouldBe JNumber(4.0)
    prism.modify(_ + 1)(JString("test")) shouldBe JString("test")
  }


}
