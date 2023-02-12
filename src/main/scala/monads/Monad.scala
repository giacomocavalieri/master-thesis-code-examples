package monads

import monads.transformers.MonadTransformer

trait Functor[F[_]]:
  extension [A](f: F[A]) def map[B](g: A => B): F[B]

trait Monad[M[_]]:
  def pure[A](a: A): M[A]
  extension [A](m: M[A]) def flatMap[B](f: A => M[B]): M[B]

object Monad:
  def pure[M[_]: Monad, A](a: A) = summon[Monad[M]].pure(a)

  given Monad[Option] with
    def pure[A](a: A): Option[A] = Some(a)
    extension [A](m: Option[A])
      def flatMap[B](f: A => Option[B]): Option[B] =
        m match
          case None    => None
          case Some(a) => f(a)

  given functorFromMonad[M[_]: Monad]: Functor[M] with
    extension [A](m: M[A])
      def map[B](g: A => B): M[B] =
        m.flatMap(a => Monad.pure(g(a)))

  extension [A, M[_]: Monad](xs: List[M[A]])
    def sequence: M[List[A]] = xs match
      case Nil => Monad.pure(Nil)
      case head :: tail =>
        for
          x  <- head
          xs <- tail.sequence
        yield x :: xs

  extension [A, M[_]: Monad](m: M[A])
    def >>[B](other: M[B]) = m.flatMap(_ => other)

    def forever[B]: M[B] = m.flatMap(_ => m.forever)

    def retry(
      times: Int,
      shouldRetry: A => Boolean
    ): M[Option[A]] =
      times match
        case 0 => Monad.pure(None)
        case n =>
          m.flatMap { result =>
            if shouldRetry(result)
            then m.retry(n - 1, shouldRetry)
            else Monad.pure(Some(result))
          }

    def lift[T[_[_], _]: MonadTransformer]: T[M, A] =
      summon[MonadTransformer[T]].lift(m)

    def void: M[Unit] = m.map(_ => ())

  def when[A, M[_]: Monad](condition: Boolean)(
    action: M[A]
  ): M[Unit] =
    if condition then action.void else Monad.pure(())
