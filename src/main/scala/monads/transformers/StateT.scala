package monads.transformers

import monads.Monad
import monads.Monad.{*, given}
import monads.transformers.MonadTransformer

final case class StateT[S, M[_], A](runStateT: S => M[(A, S)])

object StateT:
  given stateTMonad[M[_]: Monad, S]: Monad[StateT[S, M, _]] with
    def pure[A](a: A): StateT[S, M, A] =
      StateT(s => Monad.pure((a, s)))

    extension [A](t: StateT[S, M, A])
      def flatMap[B](f: A => StateT[S, M, B]): StateT[S, M, B] =
        StateT(state0 =>
          t.runStateT(state0)
            .flatMap(result =>
              val (a, state1) = result
              f(a).runStateT(state1)
            )
        )

  type StateTFixS[S] = [M[_], A] =>> StateT[S, M, A]
  given [S]: MonadTransformer[StateTFixS[S]] with
    def lift[M[_]: Monad, A](m: M[A]): StateT[S, M, A] =
      StateT(s => m.map((_, s)))

  def get[S, M[_]: Monad]: StateT[S, M[_], S] =
    StateT(s => Monad.pure((s, s)))

  def set[S, M[_]: Monad](state: S): StateT[S, M[_], Unit] =
    StateT(_ => Monad.pure(((), state)))

  object Examples:
    import monads.IO

    def stateAndIO: StateT[String, IO, Unit] =
      for
        state <- StateT.get[String, IO]
        _     <- IO.putStrLn(f"Current state: $state").lift
        _     <- IO.putStrLn("Setting new state").lift
        _     <- StateT.set[String, IO]("bar")
      yield ()

    object State:
      import monads.Identity

      type State[S, A] = StateT[S, Identity, A]

      def get[S]: State[S, S] = StateT.get
      def set[S](state: S): State[S, Unit] = StateT.set(state)

      def incrementCounter: State[Int, String] =
        for
          counter    <- State.get
          _          <- State.set(counter + 1)
          newCounter <- State.get
        yield f"counter is: $newCounter"
