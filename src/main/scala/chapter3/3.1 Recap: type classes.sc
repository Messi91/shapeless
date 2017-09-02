trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

case class Employee(name: String, number: Int, manager: Boolean)

implicit val employeeEncoder: CsvEncoder[Employee] = {
  new CsvEncoder[Employee] {
    override def encode(value: Employee): List[String] = {
      List(
        value.name,
        value.number.toString,
        if(value.manager) "yes" else "no"
      )
    }
  }
}

def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String = {
  values.map(value => enc.encode(value).mkString(",")).mkString("\n")
}

val employees: List[Employee] = List(
  Employee("Bill", 1, true),
  Employee("Peter", 2, false),
  Employee("Milton", 3, false)
)

writeCsv(employees)


case class IceCream(name: String, numCherries: Int, inCone: Boolean)

implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
  new CsvEncoder[IceCream] {
    override def encode(value: IceCream): List[String] = {
      List(
        value.name,
        value.numCherries.toString,
        if(value.inCone) "yes" else "no"
      )
    }
  }
}

val iceCreams: List[IceCream] = List(
  IceCream("Sundae", 1, false),
  IceCream("Cornetto", 0, true),
  IceCream("Banana Split", 0, false)
)

writeCsv(iceCreams)

implicit def pairEncoder[A, B](implicit aEncoder: CsvEncoder[A], bEncoder: CsvEncoder[B]) = {
  new CsvEncoder[(A, B)] {
    override def encode(pair: (A, B)) = {
      val (a, b) = pair
      aEncoder.encode(a) ++ bEncoder.encode(b)
    }
  }
}

writeCsv(employees zip iceCreams)


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


import shapeless._

CsvEncoder[IceCream]

implicitly[CsvEncoder[IceCream]]

the[CsvEncoder[IceCream]]


implicit val booleanEncoder: CsvEncoder[Boolean] = CsvEncoder.instance[Boolean] {
  (b: Boolean) => if(b) List("yes") else List("no")
}
