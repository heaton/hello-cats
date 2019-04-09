package me.heaton

import org.scalatest.{Matchers, WordSpec}

class ContravariantSpec extends WordSpec with Matchers {

  "Cats" should {
    import me.heaton.typeclass.Contravariant._

    "show money" in {
      showSalary.show(Salary(Money(1000))) shouldEqual "$1000"
    }
  }
}
