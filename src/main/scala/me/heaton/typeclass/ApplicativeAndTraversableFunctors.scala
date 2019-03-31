package me.heaton.typeclass

object ApplicativeAndTraversableFunctors {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def lift[A, B](f: A => B): F[A] => F[B] =
      fa => map(fa)(f)
  }

  implicit val functorForOption: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  trait Applicative[F[_]] extends Functor[F] {
    //    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

    def pure[A](a: A): F[A]

    //    def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
  }

  implicit class ApplicativeWrap[F[_] : Applicative, A](a: F[A]) {
    def product[B](b: F[B]): F[(A, B)] = implicitly[Applicative[F]].product(a, b)
  }

  // about ?, check https://github.com/non/kind-projector
  implicit def applicativeForEither[L]: Applicative[Either[L, ?]] = new Applicative[Either[L, ?]] {
    def product[A, B](fa: Either[L, A], fb: Either[L, B]): Either[L, (A, B)] = (fa, fb) match {
      case (Right(a), Right(b)) => Right((a, b))
      case (Left(l), _) => Left(l)
      case (_, Left(l)) => Left(l)
    }

    def pure[A](a: A): Either[L, A] = Right(a)

    def map[A, B](fa: Either[L, A])(f: A => B): Either[L, B] = fa match {
      case Right(a) => Right(f(a))
      case Left(l) => Left(l)
    }
  }

}
