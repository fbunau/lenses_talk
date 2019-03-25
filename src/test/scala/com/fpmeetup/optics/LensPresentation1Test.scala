package com.fpmeetup.optics

import cats.Id
import cats.effect.IO
import com.fpmeetup.optics.LensPresentation_1.{LensSimple, streetNameComposedLens, streetNumberComposedLens, addressLens, streetLens}
import org.scalatest._

import com.fpmeetup.optics.Data._

class LensPresentation1Test extends FreeSpec with Matchers {

  private val me = Person("Florin Bunau", Address("Cluj-Napoca", Street("victor deleu", 1)))
  private val meCapitalized = Person("Florin Bunau", Address("Cluj-Napoca", Street("Victor deleu", 1)))
  private val meOtherOffice = Person("Florin Bunau", Address("Cluj-Napoca", Street("Ferdinand 22", 1)))
  private val evilMe = Person("Florin Bunau", Address("Cluj-Napoca", Street("victor deleu", 666)))

  "Manual modification of fields using copy works as expected" in {
    me.copy(
      address = me.address.copy(
        street = me.address.street.copy(
          name = me.address.street.name.capitalize
        )
      )
    ) shouldBe meCapitalized
  }

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
    LensSimple.modify(streetNameComposedLens)(_.capitalize)(me) shouldBe meCapitalized
  }

  "Modify with failure - Success" in {
    LensSimple.modifyM(streetNameComposedLens)(a =>
      if (a.contains("v")) Some(a.capitalize) else None
    )(me) shouldBe Some(meCapitalized)
  }

  "Modify with failure - Failure" in {
    LensSimple.modifyM(streetNameComposedLens)(a =>
      if (a.contains("V")) Some(a.capitalize) else None
    )(me) shouldBe None
  }

  "Modify with IO" in {
    println("Building effect lens")

    val x = LensSimple.modifyIO(streetNameComposedLens)(a =>
      IO {
        println("Modifying street name ... ")
        a.capitalize
      }
    )(me)

    println("Done building effect lens")

    x.unsafeRunSync() shouldBe meCapitalized
  }

  "ModifyF" in {
    LensSimple.modifyF[Id, Person, String](streetNameComposedLens)(_.capitalize)(me) shouldBe meCapitalized
  }

  "ModifyF with failure - Success" in {
    import cats.instances.option._

    LensSimple.modifyF[Option, Person, String](streetNameComposedLens)(a =>
      if (a.contains("v")) Some(a.capitalize) else None
    )(me) shouldBe Some(meCapitalized)
  }

  "ModifyF with failure - Failure" in {
    import cats.instances.option._

    LensSimple.modifyF[Option, Person, String](streetNameComposedLens)(a =>
      if (a.contains("V")) Some(a.capitalize) else None
    )(me) shouldBe None
  }

  "ModifyF with IO" in {
    println("Building effect lens")

    val x = LensSimple.modifyF(streetNameComposedLens)(a =>
      IO {
        println("Modifying street name ... ")
        a.capitalize
      }
    )(me)

    println("Done building effect lens")

    x.unsafeRunSync() shouldBe meCapitalized
  }


}
