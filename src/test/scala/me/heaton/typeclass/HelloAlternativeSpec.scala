package me.heaton.typeclass

import cats.implicits._
import org.scalatest.{Matchers, WordSpec}

class HelloAlternativeSpec extends WordSpec with Matchers {

  "Alternative" should {
    import HelloAlternative._

    "pure and ap" in {
      (double.pure[Vector] <+> addFive.pure[Vector]) ap 7.pure[Vector] <+> 8.pure[Vector] shouldEqual Vector(14, 16, 12, 13)
    }

    "have decode alternative" in {
      decoderA.decode("555") shouldEqual Right(555)
      decoderA.decode("5a") shouldEqual Right(10)
    }

    "separate" in {
      partitionedResults shouldEqual(Vector((6, "Server error"), (99, "Server error"), (1200, "Bad request"), (8, "Bad request"))
        , Vector((5, 200L), (7, 200L), (22, 200L)))
    }
  }
}
