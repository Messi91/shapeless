import shapeless._

object sizeOf extends Poly1 {
  implicit val intCase: Case.Aux[Int, Int] = at(identity)

  implicit val stringCase: Case.Aux[String, Int] = at(_.length)

  implicit val booleanCase: Case.Aux[Boolean, Int] = at(bool => if (bool) 1 else 0)
}

object valueAndSizeOf extends Poly1 {
  implicit val intCase: Case.Aux[Int, Int :: Int :: HNil] =
    at(num => num :: num :: HNil)

  implicit val stringCase: Case.Aux[String, String :: Int :: HNil] =
    at(str => str :: str.length :: HNil)

  implicit val booleanCase: Case.Aux[Boolean, Boolean :: Int :: HNil] =
    at(bool => bool :: (if (bool) 1 else 0) :: HNil)
}

object sum extends Poly2 {
  implicit val intIntCase: Case.Aux[Int, Int, Int] =
    at((a, b) => a + b)

  implicit val intStringCase: Case.Aux[Int, String, Int] =
    at((a, b) => a + b.length)
}

(10 :: "hello" :: true :: HNil).map(sizeOf)

(10 :: "hello" :: true :: HNil).flatMap(valueAndSizeOf)

(10 :: "hello" :: 100 :: HNil).foldLeft(0)(sum)
