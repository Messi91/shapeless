import shapeless._
import shapeless.ops.{hlist, coproduct, nat}

val hlistLength = hlist.Length[String :: Int :: Boolean :: HNil]

val coproductLength = coproduct.Length[Double :+: Char :+: CNil]

Nat.toInt[hlistLength.Out]

Nat.toInt[coproductLength.Out]

trait SizeOf[A] {
  def value: Int
}

def sizeOf[A](implicit size: SizeOf[A]): Int = size.value

implicit def genericSizeOf[A, L <: HList, N <: Nat](
  implicit
  generic: Generic.Aux[A, L],
  size: hlist.Length.Aux[L, N],
  sizeToInt: nat.ToInt[N]
): SizeOf[A] = new SizeOf[A] {
  override def value = sizeToInt.apply()
}

case class IceCream(name: String, numCherries: Int, inCone: Boolean)

sizeOf[IceCream]
