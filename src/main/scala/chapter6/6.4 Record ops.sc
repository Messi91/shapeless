import shapeless._
import shapeless.record._

case class IceCream(name: String, numCherries: Int, inCone: Boolean)

val sundae = LabelledGeneric[IceCream].to(IceCream("Sundae", 1, false))

sundae.get('name)

sundae.get('numCherries)

sundae.updated('numCherries, 3)

sundae.remove('inCone)

sundae.updateWith('name)("MASSIVE " + _)

sundae.toMap
