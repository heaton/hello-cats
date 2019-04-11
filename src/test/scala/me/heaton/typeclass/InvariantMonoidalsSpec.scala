package me.heaton.typeclass

import org.scalatest._

class InvariantMonoidalsSpec extends WordSpec with Matchers {

  import cats.implicits._

  "Cats" should {
    import InvariantMonoidals._
    "invariant monoidal" in {
      Foo("Hello", List(0.0)) |+| Foo("World", Nil) |+| Foo("!", List(1.1, 2.2)) shouldEqual Foo("HelloWorld!", List(0.0, 1.1, 2.2))
    }
  }

  "csv" should {
    import InvariantMonoidals.csv._
    "read and write" in {
      val data = Data("foo", BinDec(10, 10), BinDec(20, 20))
      fooCodec.write(data) shouldEqual List("foo", "1010", "10", "10100", "20")
      fooCodec.read(fooCodec.write(data)) shouldEqual ((Some(data), List()))
    }
  }
}
