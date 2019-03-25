package com.fpmeetup.optics

import cats.Id
import cats.effect.IO
import org.scalatest._
import com.fpmeetup.optics.Data._
import com.fpmeetup.optics.LensPresentation_2.{LensLaarhoven, addressLens, streetLens, streetNameComposedLens, streetNumberComposedLens}

class LensPresentation2Test extends FreeSpec with Matchers {

  private val me = Person("Florin Bunau", Address("Cluj-Napoca", Street("victor deleu", 1)))
  private val meCapitalized = Person("Florin Bunau", Address("Cluj-Napoca", Street("Victor deleu", 1)))
  private val meOtherOffice = Person("Florin Bunau", Address("Cluj-Napoca", Street("Ferdinand 22", 1)))
  private  val evilMe = Person("Florin Bunau", Address("Cluj-Napoca", Street("victor deleu", 666)))

  "Lens (set) for address works as expected" in {
    addressLens.set(Address("Cluj-Napoca", Street("Victor deleu", 1)), me) shouldBe meCapitalized
  }

  "Lens (get) for address works as expected" in {
    addressLens.get(me) shouldBe me.address
  }

  "Lens (set) for street works as expected" in {
    streetLens.set(Street("Victor deleu", 1), me.address) shouldBe meCapitalized.address
  }

  "Lens (get) for street address works as expected" in {
    streetLens.get(me.address) shouldBe me.address.street
  }


  "Composed lens for street name works as expected" in {
    streetNameComposedLens.set("Ferdinand 22", me) shouldBe meOtherOffice
  }

  "Composed lens for street number works as expected" in {
    streetNumberComposedLens.set(666, me) shouldBe evilMe
  }

  "Modify" in {
    LensLaarhoven.modify(streetNameComposedLens)(_.capitalize)(me) shouldBe meCapitalized
  }


  "ModifyF" in {
    LensLaarhoven.modifyF[Id, Person, String](streetNameComposedLens)(_.capitalize)(me) shouldBe meCapitalized
  }

  "ModifyF with failure - Success" in {
    import cats.instances.option._

    LensLaarhoven.modifyF[Option, Person, String](streetNameComposedLens)(a =>
      if (a.contains("v")) Some(a.capitalize) else None
    )(me) shouldBe Some(meCapitalized)
  }

  "ModifyF with failure - Failure" in {
    import cats.instances.option._

    LensLaarhoven.modifyF[Option, Person, String](streetNameComposedLens)(a =>
      if (a.contains("V")) Some(a.capitalize) else None
    )(me) shouldBe None
  }

  "ModifyF with IO" in {
    println("Building effect lens")

    val x = LensLaarhoven.modifyF(streetNameComposedLens)(a =>
      IO {
        println("Modifying street name ... ")
        a.capitalize
      }
    )(me)

    println("Done building effect lens")

    x.unsafeRunSync() shouldBe meCapitalized
  }


}
