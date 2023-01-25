package monads.transformers

import monads.Monad
import monads.Monad.{*, given}
import monads.transformers.MonadTransformer

final case class OptionT[M[_], A](runOptionT: M[Option[A]])

object OptionT:
  given optionTMonad[M[_]: Monad]: Monad[OptionT[M, _]] with
    def pure[A](a: A): OptionT[M, A] =
      OptionT(Monad.pure(Some(a)))

    extension [A](t: OptionT[M, A])
      def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] =
        OptionT(
          t.runOptionT.flatMap(option =>
            option match
              case Some(a) => f(a).runOptionT
              case None    => Monad.pure(None)
          )
        )

  given MonadTransformer[OptionT] with
    def lift[M[_]: Monad, A](m: M[A]): OptionT[M, A] =
      OptionT(m.map(Some(_)))

  def fail[M[_]: Monad, A]: OptionT[M, A] =
    OptionT(Monad.pure(None))

  object Examples:
    import monads.IO

    def failAndIO: OptionT[IO, Unit] =
      for
        _ <- IO.putStrLn("Hello, world!").lift
        _ <- OptionT.fail: OptionT[IO, Unit]
        _ <- IO.putStrLn("Unreachable").lift
      yield ()
