import shapeless._

val hlist = 123 :: "foo" :: true :: 'x' :: HNil

hlist.apply[Nat._1]

hlist.apply(Nat._3)

hlist.take(Nat._3).drop(Nat._1)

hlist.updatedAt(Nat._1, "bar").updatedAt(Nat._2, "baz")
