package me.heaton

import org.scalatest.{Matchers, WordSpec}

class ContravariantMonoidalsSpec extends WordSpec with Matchers {

  "Contravariant Monoidal" should {
    import me.heaton.typeclass.ContravariantMonoidals._

    "check money" in {
      isEvenMoney.run(Money(55)) shouldBe false
    }

    "always even" in {
      trivial.run(5) shouldBe true
    }

    "list money" in {
      liftMoney(isEven).run(Money(55)) shouldBe false
    }

    "combine" in {
      isEvenPaymentToEvanOfMoreThan50.run(Transaction(Money(54), "Evan")) shouldBe true
    }
  }
}
