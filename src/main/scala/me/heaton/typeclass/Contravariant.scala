package me.heaton.typeclass

object Contravariant {

  import cats._

  import cats.implicits._

  case class Money(amount: Int)
  case class Salary(size: Money)

  implicit val showMoney: Show[Money] = Show.show(m => s"$$${m.amount}")

  implicit val showSalary: Show[Salary] = showMoney.contramap(_.size)

}
