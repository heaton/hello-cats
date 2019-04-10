package me.heaton.typeclass

import org.scalatest._

class InvariantsSpec extends WordSpec with Matchers {

  import cats.implicits._

  "Invariant" should {
    import Invariants._
    "convert SemiGroups" in {
      longToDate(1449088684104l) |+| longToDate(1900918893l) shouldEqual longToDate(1450989602997l)
    }
  }

}
