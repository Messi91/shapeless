import shapeless.{Coproduct, :+:, CNil, Inl, Inr}

case class Red()
case class Amber()
case class Green()

type Light = Red :+: Amber :+: Green :+: CNil

val red: Light = Inl(Red())
