package me.heaton.typeclass

// https://typelevel.org/cats/typeclasses/invariantmonoidal.html
object InvariantMonoidals {

  import cats.Semigroup

  def unit: Semigroup[Unit] = (x: Unit, y: Unit) => ()

  def product[A, B](fa: Semigroup[A], fb: Semigroup[B]): Semigroup[(A, B)] =
    (x: (A, B), y: (A, B)) => (x, y) match {
      case ((xa, xb), (ya, yb)) => fa.combine(xa, ya) -> fb.combine(xb, yb)
    }

  import cats.implicits._

  // Let's build a Semigroup for this case class
  case class Foo(a: String, c: List[Double])

  implicit val fooSemigroup: Semigroup[Foo] =
    (implicitly[Semigroup[String]], implicitly[Semigroup[List[Double]]])
      .imapN(Foo.apply)(Function.unlift(Foo.unapply))

  object csv {
    type CSV = List[String]

    trait CsvCodec[A] {
      def read(s: CSV): (Option[A], CSV)

      def write(a: A): CSV
    }

    trait CCUnit {
      def unit: CsvCodec[Unit] = new CsvCodec[Unit] {
        def read(s: CSV): (Option[Unit], CSV) = (Some(()), s)

        def write(u: Unit): CSV = List.empty
      }
    }

    trait CCProduct {
      def product[A, B](fa: CsvCodec[A], fb: CsvCodec[B]): CsvCodec[(A, B)] =
        new CsvCodec[(A, B)] {
          def read(s: CSV): (Option[(A, B)], CSV) = {
            val (a1, s1) = fa.read(s)
            val (a2, s2) = fb.read(s1)
            ((a1, a2).mapN(_ -> _), s2)
          }

          def write(a: (A, B)): CSV =
            fa.write(a._1) ++ fb.write(a._2)
        }
    }

    trait CCImap {
      def imap[A, B](fa: CsvCodec[A])(f: A => B)(g: B => A): CsvCodec[B] =
        new CsvCodec[B] {
          def read(s: CSV): (Option[B], CSV) = {
            val (a1, s1) = fa.read(s)
            (a1.map(f), s1)
          }

          def write(a: B): CSV =
            fa.write(g(a))
        }
    }

    import cats.InvariantMonoidal

    implicit val csvCodecIsInvariantMonoidal: InvariantMonoidal[CsvCodec] =
      new InvariantMonoidal[CsvCodec] with CCUnit with CCProduct with CCImap

    val stringCodec: CsvCodec[String] =
      new CsvCodec[String] {
        def read(s: CSV): (Option[String], CSV) = (s.headOption, s.drop(1))

        def write(a: String): CSV = List(a)
      }

    def numericSystemCodec(base: Int): CsvCodec[Int] =
      new CsvCodec[Int] {
        def read(s: CSV): (Option[Int], CSV) =
          (s.headOption.flatMap(head => scala.util.Try(Integer.parseInt(head, base)).toOption), s.drop(1))

        def write(a: Int): CSV =
          List(Integer.toString(a, base))
      }

    case class BinDec(binary: Int, decimal: Int)

    val binDecCodec: CsvCodec[BinDec] =
      (numericSystemCodec(2), numericSystemCodec(10)).imapN(BinDec.apply)(Function.unlift(BinDec.unapply))

    case class Data(name: String, bd1: BinDec, bd2: BinDec)

    val fooCodec: CsvCodec[Data] =
      (stringCodec, binDecCodec, binDecCodec).imapN(Data.apply)(Function.unlift(Data.unapply))

  }

}
