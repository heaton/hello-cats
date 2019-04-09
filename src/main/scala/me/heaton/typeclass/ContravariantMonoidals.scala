package me.heaton.typeclass

object ContravariantMonoidals {

  import cats._
  import cats.implicits._

  case class Predicate[A](run: A => Boolean)

  implicit val contravariantMonoidalPredicate: ContravariantMonoidal[Predicate] =
    new ContravariantMonoidal[Predicate] {
      def unit: Predicate[Unit] = Predicate[Unit](Function.const(true))

      def product[A, B](fa: Predicate[A], fb: Predicate[B]): Predicate[(A, B)] =
        Predicate(x => fa.run(x._1) && fb.run(x._2))

      def contramap[A, B](fa: Predicate[A])(f: B => A): Predicate[B] =
        Predicate(x => fa.run(f(x)))
    }

  case class Money(value: Long)

  def isEven: Predicate[Long] = Predicate(_ % 2 == 0)

  def isEvenMoney: Predicate[Money] = isEven.contramap(_.value)

  def times2Predicate: Predicate[Long] => Predicate[Long] =
    ContravariantMonoidal[Predicate].liftContravariant((x: Long) => 2 * x)

  def liftMoney: Predicate[Long] => Predicate[Money] =
    ContravariantMonoidal[Predicate].liftContravariant(_.value)

  def trivial = times2Predicate(isEven)

  // combine multiple predicates using a contramapN
  case class Transaction(value: Money, payee: String)

  def isEvan: Predicate[String] = Predicate(_ == "Evan")

  def isGreaterThan50Dollars: Predicate[Money] = liftMoney(Predicate(_ > 50))

  def isEvenPaymentToEvanOfMoreThan50 =
    (isEvenMoney, isGreaterThan50Dollars, isEvan).contramapN(
      (trans: Transaction) => (trans.value, trans.value, trans.payee))
}
