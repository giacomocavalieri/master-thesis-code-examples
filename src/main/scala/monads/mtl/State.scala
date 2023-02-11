package monads.mtl
import monads.Monad
import monads.Monad.{given, *}
import monads.transformers.{StateT, MonadTransformer, given, *}

type HasState[S] = [M[_]] =>> State[M, S]
trait State[M[_], S]:
  def get: M[S]
  def set(s: S): M[Unit]

object State:
  def apply[M[_]: HasState[S], S]: State[M, S] =
    summon[State[M, S]]

  given stateHasState[S, M[_]: Monad]: State[StateT[S, M, _], S]
    with
    def get = StateT.get
    def set(s: S) = StateT.set(s)

  // format: off
  given transformerHasState[
    T[_[_], _]: MonadTransformer,
    S, M[_]: Monad: HasState[S]]: State[T[M, _], S] with
    def get = State[M, S].get.lift[T]
    def set(s: S) = State[M, S].set(s).lift[T]
  // format: on

  object Examples:
    object WithExplicitUsing:
      def update[S, M[_]: Monad](
        f: S => S
      )(using S: State[M, S]): M[Unit] =
        for
          state <- S.get
          _     <- S.set(f(state))
        yield ()

    def update[S, M[_]: Monad: HasState[S]](f: S => S): M[Unit] =
      for
        state <- State[M, S].get
        _     <- State[M, S].set(f(state))
      yield ()
