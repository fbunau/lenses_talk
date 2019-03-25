package com.fpmeetup.optics

object Data {

  case class Person(fullName: String, address: Address)
  case class Address(city: String, street: Street)
  case class Street(name: String, number: Int)

}
