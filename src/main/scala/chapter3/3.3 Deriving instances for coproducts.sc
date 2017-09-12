import shapeless.{:+:, CNil, Coproduct, Generic, Inl, Inr}

sealed trait Shape

case class Rectangle(width: Double, height: Double) extends Shape

case class Circle(radius: Double) extends Shape


def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String = {
  values.map(value => enc.encode(value).mkString(",")).mkString("\n")
}


trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  // "Summoner" method
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = {
    enc
  }

  // "Constructor" method
  def instance[A](func: A => List[String]): CsvEncoder[A] = {
    new CsvEncoder[A] {
      override def encode(value: A): List[String] = {
        func(value)
      }
    }
  }
}

def createEncoder[A](func: A => List[String]): CsvEncoder[A] = {
  CsvEncoder.instance {
    value => func(value)
  }
}


implicit val stringEncoder: CsvEncoder[String] = createEncoder(str => List(str))

implicit val intEncoder: CsvEncoder[Int] = createEncoder(num => List(num.toString))

implicit val booleanEncoder: CsvEncoder[Boolean] = createEncoder(bool => if (bool) List("yes") else List("no"))

implicit val doubleEncoder: CsvEncoder[Double] = createEncoder(double => List(double.toString))

import shapeless.{HList, ::, HNil}

implicit val hNilEncoder: CsvEncoder[HNil] = createEncoder(_ => Nil)

implicit def hListEncoder[H, T <: HList](implicit
                                         hEncoder: CsvEncoder[H],
                                         tEncoder: CsvEncoder[T]
                                        ): CsvEncoder[H :: T] = {
  createEncoder {
    case h :: t => hEncoder.encode(h) ++ tEncoder.encode(t)
  }
}

implicit val cnilEncoder: CsvEncoder[CNil] =
  createEncoder(cnil => throw new Exception("Inconceivable!"))

implicit def coproductEncoder[H, T <: Coproduct](implicit hEncoder: CsvEncoder[H], tEncoder: CsvEncoder[T]): CsvEncoder[H :+: T] =
  createEncoder {
    case Inl(h) => hEncoder.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

implicit def genericEncoder[A, R](implicit
                                  //gen: Generic[A] {type Repr = R},
                                  gen: Generic.Aux[A, R],
                                  enc: CsvEncoder[R]): CsvEncoder[A] = {
  createEncoder[A](value => enc.encode(gen.to(value)))
}

val shapes: List[Shape] = List (
  Rectangle(3.0, 4.0),
  Circle(1.0)
)

writeCsv(shapes)
