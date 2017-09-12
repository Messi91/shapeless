import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

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

implicit val hNilEncoder: CsvEncoder[HNil] = createEncoder(_ => Nil)

implicit def hListEncoder[H, T <: HList](implicit
                                         hEncoder: Lazy[CsvEncoder[H]],
                                         tEncoder: CsvEncoder[T]
                                        ): CsvEncoder[H :: T] = {
  createEncoder {
    case h :: t => hEncoder.value.encode(h) ++ tEncoder.encode(t)
  }
}

implicit val cnilEncoder: CsvEncoder[CNil] =
  createEncoder(cnil => throw new Exception("Inconceivable!"))

implicit def coproductEncoder[H, T <: Coproduct](implicit
                                                 hEncoder: Lazy[CsvEncoder[H]],
                                                 tEncoder: CsvEncoder[T]
                                                ): CsvEncoder[H :+: T] =
  createEncoder {
    case Inl(h) => hEncoder.value.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

implicit def genericEncoder[A, R](implicit
                                  //gen: Generic[A] {type Repr = R},
                                  gen: Generic.Aux[A, R],
                                  enc: Lazy[CsvEncoder[R]]): CsvEncoder[A] = {
  createEncoder[A](value => enc.value.encode(gen.to(value)))
}


sealed trait Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case class Leaf[A](value: A) extends Tree[A]

CsvEncoder[Tree[Int]]
