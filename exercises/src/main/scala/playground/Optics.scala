package playground

object Optics {
  case class Address(streetNumber: Int, streetName: String)
  import monocle.Lens
  val streetNumber = Lens[Address, Int](_.streetNumber)(n => a => a.copy(streetNumber = n))
  streetNumber
}
