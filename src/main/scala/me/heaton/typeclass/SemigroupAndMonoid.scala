package me.heaton.typeclass

object SemigroupAndMonoid {

  trait Semigroup[A] {
    def combine(x: A, y: A): A

    def checkAssociativity(x: A, y: A, z: A): Boolean = combine(x, combine(y, z)) == combine(combine(x, y), z)
  }

  implicit val intAdditionSemigroup: Semigroup[Int] = (x: Int, y: Int) => x + y

  implicit class SemigroupWrap[A: Semigroup](a: A) {
    def |+|(b: A): A = implicitly[Semigroup[A]].combine(a, b)
  }

  def optionCombine[A: Semigroup](a: A, opt: Option[A]): A =
    opt.map(a |+| _).getOrElse(a)

  def mergeMap[K, V: Semigroup](lhs: Map[K, V], rhs: Map[K, V]): Map[K, V] =
    lhs.foldLeft(rhs) {
      case (acc, (k, v)) => acc.updated(k, optionCombine(v, acc.get(k)))
    }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A

    def checkIdentity(x: A): Boolean = combine(x, empty) == combine(empty, x) && combine(empty, x) == x
  }

  implicit val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0

    override def combine(x: Int, y: Int): Int = x + y
  }
}
