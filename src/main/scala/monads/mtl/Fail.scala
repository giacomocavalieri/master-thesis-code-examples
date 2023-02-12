package monads.mtl

import monads.{IO, Monad}
import monads.Monad.lift
import monads.transformers.{OptionT, MonadTransformer}
import monads.Identity

trait Fail[M[_]]:
  def fail[A]: M[A]

object Fail:
  def apply[M[_]: Monad: Fail]: Fail[M] = summon[Fail[M]]

  given optionCanFail[M[_]: Monad]: Fail[OptionT[M, _]] with
    def fail[A]: OptionT[M, A] = OptionT.fail

  given ioCanFail: Fail[IO] with
    def fail[A]: IO[A] = IO(() => throw Exception())

  // format: off
  given transformerCanFail[
    T[_[_],_]: MonadTransformer,
    M[_]: Monad: Fail]: Fail[T[M, _]] with
    def fail[A] = Fail[M].fail.lift[T]
  // format: on

  object Examples:
    object WithExplicitUsing:
      def divide[M[_]: Monad](dividend: Int, divisor: Int)(using
        F: Fail[M]
      ): M[Int] =
        if divisor == 0 then F.fail
        else Monad.pure(dividend / divisor)

    def divide[M[_]: Monad: Fail](
      dividend: Int,
      divisor: Int
    ): M[Int] =
      if divisor == 0 then Fail[M].fail
      else Monad.pure(dividend / divisor)

    def interpretDivide: Unit =
      val res1: Int = divide[IO](10, 20).unsafeRun()
      val res2: Option[Int] =
        divide[OptionT[Identity, _]](10, 20).runOptionT
