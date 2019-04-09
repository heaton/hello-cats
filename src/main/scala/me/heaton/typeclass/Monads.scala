package me.heaton.typeclass

object Monads {

  import cats._

  implicit def optionMonad(implicit app: Applicative[Option]) =
    new Monad[Option] {
      // Define flatMap using Option's flatten method
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
        app.map(fa)(f).flatten

      // Reuse this definition from Applicative.
      override def pure[A](a: A): Option[A] = app.pure(a)

      @annotation.tailrec
      def tailRecM[A, B](init: A)(fn: A => Option[Either[A, B]]): Option[B] =
        fn(init) match {
          case None => None
          case Some(Right(b)) => Some(b)
          case Some(Left(a)) => tailRecM(a)(fn)
        }
    }


  case class OptionT[F[_], A](value: F[Option[A]])

  implicit def optionTMonad[F[_]](implicit F : Monad[F]) = {
    new Monad[OptionT[F, ?]] {
      def pure[A](a: A): OptionT[F, A] = OptionT(F.pure(Some(a)))
      def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
        OptionT {
          F.flatMap(fa.value) {
            case None => F.pure(None)
            case Some(a) => f(a).value
          }
        }

      def tailRecM[A, B](a: A)(f: A => OptionT[F, Either[A, B]]): OptionT[F, B] =
        OptionT {
          F.tailRecM(a)(a0 => F.map(f(a0).value) {
            case None => Right[A, Option[B]](None)
            case Some(b0) => b0.map(Some(_))
          })
        }
    }
  }
}
