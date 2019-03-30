package me.heaton

import org.scalatest._

class HelloCatsSpec extends WordSpec with Matchers {
  "Type Class" should {
    import TypeClass._

    "add by Monoid" in {
      combineAll(List(Pair(1, "hello"), Pair(2, " "), Pair(3, "world"))) shouldEqual Pair(6, "hello world")
    }
  }
}
