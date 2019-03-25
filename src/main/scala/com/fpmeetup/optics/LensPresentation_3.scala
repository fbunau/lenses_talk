package com.fpmeetup.optics

import cats.data.Const
import cats.syntax.functor._
import cats.{Applicative, Functor, Id}
import com.fpmeetup.optics.Data._
import com.fpmeetup.optics.UnimaginativeData._

import scala.language.higherKinds

object LensPresentation_3 {


  // Van Laarhoven
  // like lifting the focus into the whole part
  trait LensLaarhovenP[S, T, A, B] {
    def apply[F[_]: Functor](f: A => F[B]): S => F[T]

    def set(b: B, s: S): T = {
      apply(
        _ => Applicative[Id].pure(b)
      ).apply(s)
    }

    def get(s: S): A = {
      val storedValue = apply[Const[A, ?]](a => Const(a)).apply(s)
      storedValue.getConst
    }
  }

  //////////////////////////////////////////////////////////////////////////////////

  // Some manual built lenses

  val tupleLens = new LensLaarhovenP[(Int, Int), (String, Int), Int, String] {
    override def apply[F[_] : Functor](f: Int => F[String]): ((Int, Int)) => F[(String, Int)] =
      t1 => f(t1._1).map(s => t1.copy(s, t1._2))
  }

  val addressLens = new LensLaarhovenP[Person, HomelessPerson, Address, TemporaryAddress] {
    override def apply[F[_] : Functor](f: Address => F[TemporaryAddress]): Person => F[HomelessPerson] =
      p => f(p.address).map(HomelessPerson(p.fullName, _))
  }

  val streetLens = new LensLaarhovenP[Address, TemporaryAddress, Street, Option[Shelter]] {
    override def apply[F[_] : Functor](f: Street => F[Option[Shelter]]): Address => F[TemporaryAddress] = {
      a => f(a.street).map(TemporaryAddress(a.city, _))
    }
  }

  val shelterLens = new LensLaarhovenP[Street, Option[Shelter], String, String] {
    override def apply[F[_] : Functor](f: String => F[String]): Street => F[Option[Shelter]] = {
      s => f(s.name).map(name => Some(Shelter(name)))
    }
  }

  //////////////////////////////////////////////////////////////////////////////////

  val composedLens = addressLens composeL streetLens
  val composedShelterLens = addressLens composeL streetLens composeL shelterLens

  //////////////////////////////////////////////////////////////////////////////////

  object LensLaarhovenP {

    implicit class ComposableLens[S, T, A, B](lens1: LensLaarhovenP[S, T, A, B]) {
      def composeL[C, D](lens2: LensLaarhovenP[A, B, C, D]): LensLaarhovenP[S, T, C, D] = {
        new LensLaarhovenP[S, T, C, D] {
          override def apply[F[_] : Functor](f: C => F[D]): S => F[T] = {
            lens1(lens2(f))
          }
        }
      }
    }

    //////////////////////////////////////////////////////////////////////////////////

    def modify[S, T, A, B](lens: LensLaarhovenP[S, T, A, B])(f: A => B)(s: S): T = {
      modifyF[Id, S, T, A, B](lens)(f)(s)
    }

    // more efficient than previous modify
    def modifyF[F[_]: Applicative, S, T, A, B](lens: LensLaarhovenP[S, T, A, B])(f: A => F[B])(s: S): F[T] = {
      lens(f).apply(s)
    }

  }

}
