sealed trait Shape

case class Rectangle(width: Double, height: Double) extends Shape

case class Circle(radius: Double) extends Shape


val rect: Shape = Rectangle(3.0, 4.0)

val circ: Shape = Circle(1.0)


def area(shape: Shape): Double = {
  shape match {
    case Rectangle(w, h) => w * h
    case Circle(r) => math.Pi * r * r
  }
}

area(rect)

area(circ)


type Rectangle2 = (Double, Double)

type Circle2 = Double

type Shape2 = Either[Rectangle2, Circle2]


val rect2: Rectangle2 = (3.0, 4.0)

val circ2: Circle2 = 1.0


def area(shape: Shape2): Double = {
  shape match {
    case Left((w, h)) => w * h
    case Right(r) => math.Pi * r * r
  }
}

area(Left(rect2))

area(Right(circ2))
