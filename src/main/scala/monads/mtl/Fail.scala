package monads.mtl

import monads.{IO, Monad}
import monads.Monad.lift
import monads.transformers.{OptionT, MonadTransformer}

trait Fail[M[_]]:
  def fail[A]: M[A]

object Fail:
  def apply[M[_]: Monad: Fail]: Fail[M] = summon[Fail[M]]

  given optionCanFail[M[_]: Monad]: Fail[OptionT[M, _]] with
    def fail[A] = OptionT.fail

  given ioCanFail: Fail[IO] with
    def fail[A] = IO(() => throw Exception())

  // format: off
  given transformerCanFail[
    T[_[_],_]: MonadTransformer,
    M[_]: Monad: Fail]: Fail[T[M, _]] with
    def fail[A] = Fail[M].fail.lift[T]
  // format: on
