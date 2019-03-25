package com.fpmeetup.optics

import cats.data.Const
import cats.{Applicative, Functor, Id}
import com.fpmeetup.optics.Data._
import cats.syntax.functor._
import com.fpmeetup.optics.LensPresentation_1.addressLens

import scala.language.higherKinds

object LensPresentation_2 {

  // Van Laarhoven
  // like lifting the focus into the whole part
  trait LensLaarhoven[S, A] {
    def apply[F[_]: Functor](f: A => F[A]): S => F[S]

    def set(a: A, s: S): S = {
      apply(
        _ => Applicative[Id].pure(a) // ignores the argument, otherwise we would get a modifier
      ).apply(s)
    }

    def get(s: S): A = {
      // to get from F[S] to an A, you put the A in F
      // we use the Const to make an F that has an A

      // then we have S => F[S], with a clojure of F
      // S => Const[A, S]

      // (A => Const[A, A]) => S => Const[A, S]
      // S => Const[A, S]
      // Const[A, S]
      val storedValue = apply[Const[A, ?]](a => Const(a)).apply(s)
      storedValue.getConst
    }
  }

  //////////////////////////////////////////////////////////////////////////////////

  // Some manual built lenses

  val addressLens = new LensLaarhoven[Person, Address] {
    override def apply[F[_] : Functor](f: Address => F[Address]): Person => F[Person] = {
      p: Person => f(p.address).map(x => p.copy(address = x))
    }
  }

  val streetLens = new LensLaarhoven[Address, Street] {
    override def apply[F[_] : Functor](f: Street => F[Street]): Address => F[Address] = {
      a: Address => f(a.street).map(x => a.copy(street = x))
    }
  }

  val nameLens = new LensLaarhoven[Street, String] {
    override def apply[F[_] : Functor](f: String => F[String]): Street => F[Street] = {
      s: Street => f(s.name).map(x => s.copy(name = x))
    }
  }

  val numberLens = new LensLaarhoven[Street, Int] {
    override def apply[F[_] : Functor](f: Int => F[Int]): Street => F[Street] = {
      s: Street => f(s.number).map(x => s.copy(number = x))
    }
  }

  //////////////////////////////////////////////////////////////////////////////////

  // Composed lens
  val streetNameComposedLens: LensLaarhoven[Person, String] = addressLens composeL streetLens composeL nameLens
  val streetNumberComposedLens: LensLaarhoven[Person, Int] = addressLens composeL streetLens composeL numberLens

  //////////////////////////////////////////////////////////////////////////////////

  object LensLaarhoven {

    implicit class ComposableLens[S, A](lens1: LensLaarhoven[S, A]) {
      def composeL[B](lens2: LensLaarhoven[A, B]): LensLaarhoven[S, B] = {
        new LensLaarhoven[S, B] {
          override def apply[F[_] : Functor](f: B => F[B]): S => F[S] = {
            lens1(lens2(f))
          }
        }
      }
    }

    //////////////////////////////////////////////////////////////////////////////////

    def modify[S, A](lens: LensLaarhoven[S, A])(f: A => A)(s: S): S = {
      modifyF[Id, S, A](lens)(f)(s)
    }

    // more efficient than previous modify
    def modifyF[F[_]: Applicative, S, A](lens: LensLaarhoven[S, A])(f: A => F[A])(s: S): F[S] = {
      lens(f).apply(s)
    }

  }

  def main(args: Array[String]): Unit = {
    val c = Const[Int, String] _
    val x = c.apply(1)
    println(x.getConst)

    val y = x.map(_.toDouble)
    println(y.getConst)

    // Const just eats a map
  }

}
