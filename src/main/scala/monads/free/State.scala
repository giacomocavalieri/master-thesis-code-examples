package monads.free

import monads.Monad.{given, *}
import monads.free.lib.*

enum StateDSL[S, A]:
  case Get[S]() extends StateDSL[S, S]
  case Set[S](s: S) extends StateDSL[S, Unit]

type State[S, A] = Program[StateDSL[S, _], A]

object State:
  import StateDSL.*
  def get[S]: State[S, S] = Program.fromInstruction(Get())
  def set[S](s: S): State[S, Unit] =
    Program.fromInstruction(Set(s))

object Examples:
  import StateDSL.*
  import State.*
  import monads.transformers.StateT
  import monads.Identity

  def pureInterpreter[S] =
    new (StateDSL[S, _] ~> StateT[S, Identity, _]):
      def apply[A](s: StateDSL[S, A]): StateT[S, Identity, A] =
        s match
          case Get()  => StateT.get
          case Set(s) => StateT.set(s)

  def impureInterpreter[S](initialState: S) =
    new (StateDSL[S, _] ~> Identity):
      var currentState = initialState
      def apply[A](s: StateDSL[S, A]): Identity[A] =
        s match
          case Get()  => Identity(currentState)
          case Set(s) => currentState = s; Identity(())

  def incrementCounter: State[Int, Int] = for
    n <- get
    _ <- set(n + 1)
    m <- get
  yield m

  @main def prova =
    println(
      incrementCounter.interpret(pureInterpreter).runStateT(10)
    )

    println(incrementCounter.interpret(impureInterpreter(10)))
