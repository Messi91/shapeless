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


case class Employee(name: String, number: Int, manager: Boolean)

case class IceCream(name: String, numCherries: Int, inCone: Boolean)


implicit val stringEncoder: CsvEncoder[String] = createEncoder(str => List(str))

implicit val intEncoder: CsvEncoder[Int] = createEncoder(num => List(num.toString))

implicit val booleanEncoder: CsvEncoder[Boolean] = createEncoder(bool => if (bool) List("yes") else List("no"))


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

val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

reprEncoder.encode("abc" :: 123 :: true :: HNil)



import shapeless.Generic

implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
  val gen = Generic[IceCream]
  val enc = CsvEncoder[gen.Repr]
  createEncoder[IceCream](iceCream => enc.encode(gen.to(iceCream)))
}

def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String = {
  values.map(value => enc.encode(value).mkString(",")).mkString("\n")
}

val iceCreams: List[IceCream] = List(
  IceCream("Sundae", 1, false),
  IceCream("Cornetto", 0, true),
  IceCream("Banana Split", 0, false)
)

writeCsv(iceCreams)


implicit def genericEncoder[A, R](implicit
                                  //gen: Generic[A] {type Repr = R},
                                  gen: Generic.Aux[A, R],
                                  enc: CsvEncoder[R]): CsvEncoder[A] = {
  createEncoder[A](value => enc.encode(gen.to(value)))
}

val employees: List[Employee] = List(
  Employee("Bill", 1, true),
  Employee("Peter", 2, false),
  Employee("Milton", 3, false)
)

writeCsv(employees)
