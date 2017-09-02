import shapeless.{:+:, CNil, Inl, Inr}

case class Red()
case class Amber()
case class Green()

type Light = Red :+: Amber :+: Green :+: CNil

val red: Light = Inl(Red())

val amber: Light = Inr(Inl(Amber()))

val green: Light = Inr(Inr(Inl(Green())))


import shapeless.Generic

sealed trait Shape
case class Rectangle(width: Double, height: Double) extends Shape
case class Circle(radius: Double) extends Shape

val gen = Generic[Shape]

gen.to(Rectangle(3.0, 4.0))

gen.to(Circle(1.0))
