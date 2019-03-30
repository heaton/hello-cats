package me.heaton

import org.scalatest._

class HelloCatsSpec extends WordSpec with Matchers {
  "Type Class" should {
    import me.heaton.typeclass.TypeClass._

    "add by Monoid" in {
      combineAll(List(Pair(1, "hello"), Pair(2, " "), Pair(3, "world"))) shouldEqual Pair(6, "hello world")
    }
  }

  "Semigroup and Monoid" should {
    import me.heaton.typeclass.SemigroupAndMonoid._
    "Semigroup should be associative" in {
      intAdditionSemigroup.checkAssociativity(1, 2, 3) shouldBe true
    }
    "Monoid should be identical" in {
      intAdditionMonoid.checkIdentity(1) shouldBe true
    }

    "mergeMap with optionCombine" in {
      val map1 = Map("hello" -> 0, "world" -> 1)
      val map2 = Map("hello" -> 2, "cats" -> 3)
      mergeMap(map1, map2) shouldEqual Map("hello" -> 2, "cats" -> 3, "world" -> 1)
    }
  }

  "Cats" should {
    import cats.kernel.Monoid
    import cats.implicits._

    "support combine |+| for Map" in {
      val map1 = Map("hello" -> 0, "world" -> 1)
      val map2 = Map("hello" -> 2, "cats" -> 3)
      map1 |+| map2 shouldEqual Map("hello" -> 2, "cats" -> 3, "world" -> 1)
    }

    "support combineAll" in {
      Monoid.combineAll(List(1, 2, 3)) shouldBe 6
      Monoid.combineAll(List(Set(1, 2), Set(3, 4, 5))) shouldBe Set(1, 2, 3, 4, 5)
    }
  }
}
