package monads

import monads.Monad.{*, given}

final case class State[S, A](runState: S => (A, S))

object State:
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  given [S]: Monad[State[S, _]] with
    def pure[A](a: A): State[S, A] = State(s => (a, s))
    extension [A](m: State[S, A])
      def flatMap[B](f: A => State[S, B]): State[S, B] =
        State(state0 =>
          val (result1, state1) = m.runState(state0)
          f(result1).runState(state1)
        )

  object Examples:
    def incrementCounter: State[Int, String] =
      get.flatMap(counter =>
        set(counter + 1).flatMap(_ =>
          get.flatMap(newCounter =>
            Monad.pure(f"counter is: $newCounter")
          )
        )
      )

    def incrementCounterForComprehension: State[Int, String] =
      for
        counter    <- State.get
        _          <- State.set(counter + 1)
        newCounter <- State.get
      yield f"counter is: $newCounter"
