package monads.transformers

import monads.Monad
import monads.Monad.{*, given}

trait MonadTransformer[T[_[_], _]]:
  extension [M[_]: Monad, A](m: M[A]) def lift: T[M, A]

object Examples:
  final case class T[M[_], A]()

  given MonadTransformer[T] with
    extension [M[_]: Monad, A](m: M[A]) def lift: T[M, A] = ???

  given transformerInstance[M[_]: Monad]: Monad[T[M, _]] with
    def pure[A](a: A): T[M, A] = ???
    extension [A](t: T[M, A])
      def flatMap[B](f: A => T[M, B]): T[M, B] = ???
