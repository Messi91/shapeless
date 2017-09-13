import shapeless._
import shapeless.ops.coproduct
import shapeless.ops.nat.ToInt

trait Random[A] {
  def get: A
}

def random[A](implicit r: Random[A]): A = r.get

def createRandom[A](func: () => A): Random[A] = {
  new Random[A] {
    override def get = func()
  }
}

implicit val intRandom: Random[Int] = createRandom(() => scala.util.Random.nextInt(10))

implicit val charRandom: Random[Char] = createRandom(() => ('A'.toInt + scala.util.Random.nextInt(26)).toChar)

implicit val booleanRandom: Random[Boolean] = createRandom(() => scala.util.Random.nextBoolean)

for(i <- 1 to 3) println(random[Int])

for(i <- 1 to 3) println(random[Char])


implicit def genericRandom[A, R](implicit
                                 gen: Generic.Aux[A, R],
                                 random: Lazy[Random[R]]): Random[A] = {
  createRandom(() => gen.from(random.value.get))
}

implicit val hnilRandom: Random[HNil] = createRandom(() => HNil)

implicit def hlistRandom[H, T <: HList](implicit
                                        hRandom: Lazy[Random[H]],
                                        tRandom: Random[T]): Random[H :: T] = {
  createRandom(() => hRandom.value.get :: tRandom.get)
}

case class Cell(col: Char, row: Int)

for(i <- 1 to 5) println(random[Cell])


implicit val cnilRandom: Random[CNil] =
  createRandom(() => throw new Exception("Inconceivable!"))

implicit def coproductRandom[H, T <: Coproduct, L <: Nat](implicit
                                                          hRandom: Lazy[Random[H]],
                                                          tRandom: Random[T],
                                                          tLength: coproduct.Length.Aux[T, L],
                                                          tLengthAsInt: ToInt[L]
                                                         ): Random[H :+: T] = {
  createRandom { () =>
    val length = 1 + tLengthAsInt()
    val chooseH = scala.util.Random.nextDouble < (1.0 / length)
    if(chooseH) Inl(hRandom.value.get) else Inr(tRandom.get)
  }
}

sealed trait Light
case object Red extends Light
case object Amber extends Light
case object Green extends Light

for(i <- 1 to 100) println(random[Light])
