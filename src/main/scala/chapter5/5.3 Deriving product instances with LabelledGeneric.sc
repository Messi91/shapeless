sealed trait JsonValue

case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue

case class JsonArray(items: List[JsonValue]) extends JsonValue

case class JsonString(value: String) extends JsonValue

case class JsonNumber(value: Double) extends JsonValue

case class JsonBoolean(value: Boolean) extends JsonValue

case object JsonNull extends JsonValue


trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}

object JsonEncoder {
  def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc
}


def createEncoder[A](func: A => JsonValue): JsonEncoder[A] = new JsonEncoder[A] {
  override def encode(value: A) = func(value)
}

implicit val stringEncoder: JsonEncoder[String] = createEncoder(str => JsonString(str))

implicit val doubleEncoder: JsonEncoder[Double] = createEncoder(num => JsonNumber(num))

implicit val intEncoder: JsonEncoder[Int] = createEncoder(num => JsonNumber(num))

implicit val booleanEncoder: JsonEncoder[Boolean] = createEncoder(bool => JsonBoolean(bool))

implicit def listEncoder[A](enc: JsonEncoder[A]): JsonEncoder[List[A]] = {
  createEncoder(list => JsonArray(list.map(item => enc.encode(item))))
}

implicit def optionEncoder[A](enc: JsonEncoder[A]): JsonEncoder[Option[A]] = {
  createEncoder(opt => opt.map(value => enc.encode(value)).getOrElse(JsonNull))
}


case class IceCream(name: String, numCherries: Int, inCone: Boolean, person: Person)

case class Person(firstName: String, lastName: String)

val iceCream = IceCream("Sundae", 1, false, Person("Mesfin", "Mebrate"))

val iceCreamJson: JsonValue = JsonObject(List(
  "name" -> JsonString("Sundae"),
  "numCherries" -> JsonNumber(1),
  "inCone" -> JsonBoolean(false)
))

import shapeless.{LabelledGeneric, Witness}

val gen = LabelledGeneric[IceCream].to(iceCream)


trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(value: A): JsonObject
}

def createObjectEncoder[A](fn: A => JsonObject): JsonObjectEncoder[A] = {
  new JsonObjectEncoder[A] {
    override def encode(value: A) = fn(value)
  }
}


import shapeless.{HList, ::, HNil, Lazy}
import shapeless.LabelledGeneric
import shapeless.labelled.FieldType

implicit val hnilEncoder: JsonObjectEncoder[HNil] = {
  createObjectEncoder(_ => JsonObject(Nil))
}

implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](implicit
                                               witness: Witness.Aux[K],
                                               hEncoder: Lazy[JsonEncoder[H]],
                                               tEncoder: JsonObjectEncoder[T]
                                              ): JsonObjectEncoder[FieldType[K, H] :: T] = {
  val fieldName: String = witness.value.name
  createObjectEncoder { hlist =>
    val head = hEncoder.value.encode(hlist.head)
    val tail = tEncoder.encode(hlist.tail)
    JsonObject((fieldName, head) :: tail.fields)
  }
}

implicit def genericObjectEncoder[A, H <: HList](implicit
                                                 generic: LabelledGeneric.Aux[A, H],
                                                 hEncoder: Lazy[JsonObjectEncoder[H]]
                                                ): JsonObjectEncoder[A] = {
  createObjectEncoder { value =>
    hEncoder.value.encode(generic.to(value))
  }
}

JsonEncoder[IceCream].encode(iceCream)

def writeJson[A](item: A)(implicit enc: JsonEncoder[A]): String = {
  def writeJson(jsonValue: JsonValue): String = {
    jsonValue match {
      case JsonString(string) => s""""$string"""".stripMargin
      case JsonNumber(number) => number.toString
      case JsonBoolean(boolean) => boolean.toString
      case JsonArray(items) => items.map(item => writeJson(item)).mkString("[", ", ", "]")
      case JsonNull => ""
      case JsonObject(fields) => fields.map { case (key, value) =>
        s""""$key": ${writeJson(value)}"""
      }.mkString("{", ", ", "}")
    }
  }

  writeJson(enc.encode(item))
}

writeJson(iceCream)