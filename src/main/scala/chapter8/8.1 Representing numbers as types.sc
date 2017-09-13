import shapeless.{Nat, Succ}
import shapeless.ops.nat.ToInt

type Zero = Nat._0
type One = Succ[Zero]
type Two = Succ[One]

Nat._1
Nat._2
Nat._3

val toInt = ToInt[Two]

toInt.apply()

Nat.toInt(Nat._3)
