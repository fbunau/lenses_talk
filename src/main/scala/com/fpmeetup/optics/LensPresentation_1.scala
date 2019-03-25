package com.fpmeetup.optics

import cats.Functor
import cats.effect.IO
import cats.syntax.functor._
import com.fpmeetup.optics.Data._

import scala.language.higherKinds

object LensPresentation_1 {

  trait LensSimple[S, A] {
    def get(s: S): A
    def set(a: A, s: S): S
  }


  // Some manual built lenses

  val addressLens = new LensSimple[Person, Address] {
    override def get(s: Person): Address = s.address
    override def set(a: Address, s: Person): Person = s.copy(address = a)
  }

  val streetLens = new LensSimple[Address, Street] {
    override def get(s: Address): Street = s.street
    override def set(a: Street, s: Address): Address = s.copy(street = a)
  }

  val nameLens = new LensSimple[Street, String] {
    override def get(s: Street): String = s.name
    override def set(a: String, s: Street): Street = s.copy(name = a)
  }

  val numberLens = new LensSimple[Street, Int] {
    override def get(s: Street): Int = s.number
    override def set(a: Int, s: Street): Street = s.copy(number = a)
  }


  //////////////////////////////////////////////////////////////////////////////////


  // Composed lens
  val streetNameComposedLens: LensSimple[Person, String] = addressLens composeL streetLens composeL nameLens
  val streetNumberComposedLens: LensSimple[Person, Int] = addressLens composeL streetLens composeL numberLens


  //////////////////////////////////////////////////////////////////////////////////


  object LensSimple {

    implicit class ComposableLens[S, A](lens1: LensSimple[S, A]) {

      def composeL[B](lens2: LensSimple[A, B]): LensSimple[S, B] = {
        new LensSimple[S, B] {

          override def get(s: S): B =
            lens2.get(lens1.get(s))

          override def set(b: B, s: S): S = {
            lens1.set(
              lens2.set(b, lens1.get(s)), s
            )
          }
        }
      }

    }


    //////////////////////////////////////////////////////////////////////////////////

    // not very efficient
    def modify[S, A](lens: LensSimple[S, A])(f: A => A)(s: S): S =
      lens.set(f(lens.get(s)), s)


    def modifyM[S, A](lens: LensSimple[S, A])(f: A => Option[A])(s: S): Option[S] =
      f(lens.get(s)).map(
        lens.set(_, s)
      )

    def modifyIO[S, A](lens: LensSimple[S, A])(f: A => IO[A])(s: S): IO[S] = {
      f(lens.get(s)).map(
        lens.set(_, s)
      )
    }

    def modifyF[F[_] : Functor, S, A](lens: LensSimple[S, A])(f: A => F[A])(s: S): F[S] = {
      f(lens.get(s)).map(
        lens.set(_, s)
      )
    }


    ///////////////////////////////

  }


}
