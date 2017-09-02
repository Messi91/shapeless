import shapeless.{HList, ::, HNil}

val product: String :: Int :: Boolean :: HNil = "Sunday" :: 1 :: false :: HNil

val first = product.head

val second = product.tail.head

val rest = product.tail.tail

val newProduct: ::[Long, ::[String, ::[Int, ::[Boolean, HNil]]]] = 42L :: product


import shapeless.Generic

case class IceCream(name: String, numCherries: Int, inCone: Boolean)

val iceCreamGen = Generic[IceCream]

val iceCream = IceCream("Sundae", 1, false)

val repr = iceCreamGen.to(iceCream)

val iceCream2 = iceCreamGen.from(repr)


case class Employee(name: String, number: Int, manager: Boolean)

val employee = Generic[Employee].from(Generic[IceCream].to(iceCream))


val tupleGen = Generic[(String, Int, Boolean)]

tupleGen.to(("Hello", 123, true))

tupleGen.from("Hello" :: 123 :: true :: HNil)


case class BigData(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int,
                   i: Int, j: Int, k: Int, l: Int, m: Int, n: Int, o: Int, p: Int,
                   q: Int, r: Int, s: Int, t: Int, u: Int, v: Int, w: Int)

Generic[BigData].from(Generic[BigData].to(
  BigData(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    22, 23)))
