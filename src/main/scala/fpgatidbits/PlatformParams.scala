package fpgatidbits


// Erlingrj: Experimental feature.
//  Add a global PlatformParams which can be
//  an implicit parameter to modules. In that way
//  you wont have to pass the parameters around everywhere

case class PlatformParams
(
  val useNativeChiselQueue : Boolean = false
)