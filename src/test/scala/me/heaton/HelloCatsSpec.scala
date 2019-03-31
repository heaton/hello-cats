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

  "Applicative and Traverable Functors" should {
    import me.heaton.typeclass.ApplicativeAndTraversableFunctors._

    "Functor" should {
      "follow composition fa.map(f).map(g) = fa.map(f.andThen(g))" in {
        val option = Some(1)
        val f: Int => Int = _ * 2
        val g: Int => Int = _ * 3
        functorForOption.map(functorForOption.map(option)(f))(g) shouldEqual functorForOption.map(option)(f.andThen(g)(_))
      }

      "follow identity fa.map(x => x) = fa" in {
        val option = Some(1)
        functorForOption.map(option)(x => x) shouldEqual option
      }

      "Applicative" should {
        "follow Associativity fa.product(fb).product(fc) ~ fa.product(fb.product(fc))" in {
          val fa: Either[Nothing, Int] = Right(1)
          val fb: Either[Nothing, Int] = Right(2)
          val fc: Either[Nothing, Int] = Right(3)
          fa.product(fb).product(fc) shouldEqual fa.product(fb.product(fc)).map { case (a, (b, c)) => ((a, b), c) }
        }

        "follow Left identity pure(()).product(fa).map(_._2) = fa" in {
          val fa: Either[Nothing, Int] = Right(1)
          applicativeForEither.pure(()).product(fa).map(_._2) shouldEqual fa
        }

        "follow Right Identity fa.product(pure(())) ~ fa" in {
          val fa: Either[Nothing, Int] = Right(1)
          fa.product(applicativeForEither.pure(())).map(_._1) shouldEqual fa
        }
      }
    }
  }

  "Cats" should {
    import cats.implicits._

    "Monoid" should {
      import cats.kernel.Monoid
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

    "Functor" should {
      import cats.Functor
      import cats.data.Nested

      "support compose" in {
        val listOption = List(Some(1), None, Some(2))
        Functor[List].compose[Option].map(listOption)(_ + 1) shouldEqual List(Some(2), None, Some(3))
      }

      "be able to use Nested to automatically compose" in {
        val listOption = List(Some(1), None, Some(2))
        Nested(listOption) map (_ + 1) shouldEqual Nested(List(Some(2), None, Some(3)))
      }
    }

    "Applicative" should {
      import cats.Applicative
      import cats.data.Nested
      import cats.implicits._
      import scala.concurrent.Future
      import scala.concurrent.ExecutionContext.Implicits.global

      def product3[F[_] : Applicative, A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] = {
        val F = Applicative[F]
        val fabc = F.product(F.product(fa, fb), fc)
        F.map(fabc) { case ((a, b), c) => (a, b, c) }
      }

      "create triple by product3" in {
        val fa: Either[Nothing, Int] = Right(1)
        val fb: Either[Nothing, Int] = Right(2)
        val fc: Either[Nothing, Int] = Right(3)
        product3(fa, fb, fc) shouldEqual Right((1, 2, 3))
      }

      "compose" in {
        val x: Future[Option[Int]] = Future.successful(Some(5))
        val y: Future[Option[Char]] = Future.successful(Some('a'))
        Applicative[Future].compose[Option].map2(x, y)(_ + _) map {
          _ shouldEqual Some(102)
        }
        Nested(x).map2(Nested(y))(_ + _).value map {
          _ shouldEqual Some(102)
        }
      }

      "Traverse Concept" should {
        import java.sql.Connection
        import cats.implicits._

        val username: Option[String] = Some("username")
        val password: Option[String] = Some("password")
        val url: Option[String] = Some("some.login.url.here")

        // Stub for demonstration purposes
        def attemptConnect(username: String, password: String, url: String): Option[Connection] = None

        "map3" in {
          Applicative[Option].map3(username, password, url)(attemptConnect) shouldEqual Some(None)
          (username, password, url).mapN(attemptConnect) shouldEqual Some(None)
        }

        def traverse[F[_] : Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
          as.foldRight(Applicative[F].pure(List.empty[B])) { (a: A, acc: F[List[B]]) =>
            val fb: F[B] = f(a)
            Applicative[F].map2(fb, acc)(_ :: _)
          }

        "traverse a list" in {
          traverse(List(1, 2, 3))(i => Some(i): Option[Int]) shouldEqual Some(List(1, 2, 3))
        }

        "traverse and sequence" in {
          val list = List(Some(1), Some(2), None)
          list.traverse(identity) shouldEqual list.sequence
          val f: Option[Int] => Option[Int] = _.orElse(Some(0))
          list.traverse(f) shouldEqual list.map(f).sequence
        }
      }
    }
  }
}
