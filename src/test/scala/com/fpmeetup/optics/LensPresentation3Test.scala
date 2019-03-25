package com.fpmeetup.optics

import cats.effect.IO
import com.fpmeetup.optics.Data._
import com.fpmeetup.optics.UnimaginativeData._
import com.fpmeetup.optics.LensPresentation_3._
import org.scalatest._

class LensPresentation3Test extends FreeSpec with Matchers {

  private val me = Person("Florin Bunau", Address("Cluj-Napoca", Street("victor deleu", 1)))
  private val homelessMe = HomelessPerson("Florin Bunau", TemporaryAddress("Cluj-Napoca", None))
  private val homelessMeWithAJob = HomelessPerson("Florin Bunau", TemporaryAddress("Cluj-Napoca", Some(Shelter("Victor deleu"))))

  private val t1 = (666, 42)
  private val t2 = ("evil", 42)

  "Lens (set) for tuple" in {
    tupleLens.set("evil", t1) shouldBe t2
  }

  "Lens (set) for address works as expected" in {
    addressLens.set(TemporaryAddress("Cluj-Napoca", None), me) shouldBe homelessMe
  }

  "Lens (get) for address works as expected" in {
    addressLens.get(me) shouldBe me.address
  }

  "Lens (set) for street works as expected" in {
    streetLens.set(None, me.address) shouldBe homelessMe.address
  }

  "Lens (get) for street address works as expected" in {
    streetLens.get(me.address) shouldBe me.address.street
  }


  "Composed lens for street name works as expected" in {
    composedLens.set(None, me) shouldBe homelessMe
  }

  "Modify" in {
    LensLaarhovenP.modify(composedShelterLens)(_.capitalize)(me) shouldBe homelessMeWithAJob
  }


  "ModifyF with IO" in {
    println("Building effect lens")

    val x = LensLaarhovenP.modifyF(composedShelterLens)(a =>
      IO {
        println("Sleeping at my job ... ")
        a.capitalize
      }
    )(me)

    println("Done building effect lens")

    x.unsafeRunSync() shouldBe homelessMeWithAJob
  }


}
