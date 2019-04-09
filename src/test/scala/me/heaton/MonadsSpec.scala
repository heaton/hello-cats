package me.heaton

import org.scalatest.{Matchers, WordSpec}

class MonadsSpec extends WordSpec with Matchers {

  "Monad" should {
    import me.heaton.typeclass.Monads._
    import cats.implicits._
    "have flatMap" in {
      optionMonad.flatMap(Some(1))(a => Some(a + 1)) shouldEqual Some(2)
    }

    "have tailRecM" in {
      optionMonad.tailRecM(100) {
        case 0 => Some(Right("OK"))
        case a => Some(Left(a - 1))
      } shouldEqual Some("OK")
    }

    "use optionT" in {
      optionTMonad[List].pure(1).value shouldEqual List(Some(1))
    }
  }

  "Cats" should {
    import cats.implicits._
    "have ifM on Monad" in {
      List(true, false, true).ifM(ifTrue = List(1, 2), ifFalse = List(3, 4)) shouldEqual List(1, 2, 3, 4, 1, 2)
    }
  }
}
