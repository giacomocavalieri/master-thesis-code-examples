package monads.free

import monads.Monad.{given, *}
import monads.free.lib.*
import scala.annotation.tailrec

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

  extension [S, A](program: Program[StateDSL[S, _], A])
    @tailrec def runWithState(state: S): (S, A) =
      program.next match
        case ProgramView.Return(value) => (state, value)
        case ProgramView.Then(instruction, continuation) =>
          instruction match
            case Get() => continuation(state).runWithState(state)
            case Set(s) => continuation(()).runWithState(s)

  @main
  def runProgram =
    val program = for
      n <- get[Int]
      _ <- set(n + 1)
    yield f"State was $n"
    println(program.runWithState(0))
    // -> (1, "State was 0")
