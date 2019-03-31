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
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ap(map(fa)(a => (b: B) => (a, b)))(fb)

    def pure[A](a: A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
  }

  implicit class ApplicativeWrap[F[_] : Applicative, A](a: F[A]) {
    def product[B](b: F[B]): F[(A, B)] = implicitly[Applicative[F]].product(a, b)
  }

  // about ?, check https://github.com/non/kind-projector
  implicit def applicativeForEither[L]: Applicative[Either[L, ?]] = new Applicative[Either[L, ?]] {

    def ap[A, B](ff: Either[L, A => B])(fa: Either[L, A]): Either[L, B] = (ff, fa) match {
      case (Right(f), Right(a)) => pure(f(a))
      case (Left(l), _) => Left(l)
      case (_, Left(l)) => Left(l)
    }

    def pure[A](a: A): Either[L, A] = Right(a)

  }

  // Traversables are Functors
  // https://typelevel.org/cats/typeclasses/traverse.html
  final case class Id[A](value: A)

  implicit val applicativeForId: Applicative[Id] = new Applicative[Id] {
    def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] = Id(ff.value(fa.value))

    def pure[A](a: A): Id[A] = Id(a)
  }

  trait Traverse[F[_]] extends Functor[F] {
    def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

    def map[A, B](fa: F[A])(f: A => B): F[B] = traverse(fa)(a => Id(f(a))).value
  }

}
