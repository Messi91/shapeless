import cats.Monoid
import cats.instances.all._
import shapeless.labelled.{field, FieldType}
import shapeless._
import shapeless.ops.hlist

case class IceCreamV1(name: String, numCherries: Int, inCone: Boolean)

case class IceCreamV2a(name: String, inCone: Boolean)

case class IceCreamV2b(name: String, inCone: Boolean, numCherries: Int)

case class IceCreamV2c(name: String, inCone: Boolean, numCherries: Int, numWaffles: Int)

trait Migration[A, B] {
  def apply(a: A): B
}

implicit class MigrationOps[A](a: A) {
  def migrateTo[B](implicit migration: Migration[A, B]): B = {
    migration.apply(a)
  }
}

def createMonoid[A](zero: A)(add: (A, A) => A): Monoid[A] = {
  new Monoid[A] {
    override def empty = zero
    override def combine(x: A, y: A): A = add(x, y)
  }
}

implicit val hnilMonoid: Monoid[HNil] = createMonoid[HNil](HNil)((_, _) => HNil)

implicit def emptyHList[K <: Symbol, H, T <: HList](
  implicit
  hMonoid: Lazy[Monoid[H]],
  tMonoid: Monoid[T]
): Monoid[FieldType[K, H] :: T] = {
  createMonoid(field[K](hMonoid.value.empty) :: tMonoid.empty) {
    (x, y) =>
      field[K](hMonoid.value.combine(x.head, y.head)) ::
        tMonoid.combine(x.tail, y.tail)
  }
}

implicit def genericMigration[
  A, B, ARepr <: HList, BRepr <: HList,
  Common <: HList, Added <: HList, Unaligned <: HList
](
  implicit
  aGen: LabelledGeneric.Aux[A, ARepr],
  bGen: LabelledGeneric.Aux[B, BRepr],
  inter: hlist.Intersection.Aux[ARepr, BRepr, Common],
  diff: hlist.Diff.Aux[BRepr, Common, Added],
  monoid: Monoid[Added],
  prepend: hlist.Prepend.Aux[Added, Common, Unaligned],
  align: hlist.Align[Unaligned, BRepr]
): Migration[A, B] = new Migration[A, B] {
  override def apply(a: A): B = bGen.from(
    align.apply(
      prepend.apply(
        monoid.empty,
        inter.apply(
          aGen.to(a)
        )
      )
    )
  )
}

IceCreamV1("Sundae", 1, false)

IceCreamV1("Sundae", 1, false).migrateTo[IceCreamV2a]

IceCreamV1("Sundae", 1, false).migrateTo[IceCreamV2b]

IceCreamV1("Sundae", 1, false).migrateTo[IceCreamV2c]













































