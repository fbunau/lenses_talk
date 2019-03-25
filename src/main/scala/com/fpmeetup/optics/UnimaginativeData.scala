package com.fpmeetup.optics

object UnimaginativeData {

  case class HomelessPerson(name: String, address: TemporaryAddress)
  case class TemporaryAddress(city: String, shelter: Option[Shelter])
  case class Shelter(name: String)

}
