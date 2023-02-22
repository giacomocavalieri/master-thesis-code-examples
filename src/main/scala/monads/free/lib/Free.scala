package monads.free.lib

import monads.Functor
import monads.Monad
import monads.Monad.{*, given}
import scala.annotation.tailrec

// Program is a "Free Operational Monad"
enum Program[I[_], A]:
  case Instruction[I[_], A](instruction: I[A])
      extends Program[I, A]
  case Return[I[_], A](value: A) extends Program[I, A]
  case Then[I[_], A, B](
    program: Program[I, A],
    continuation: A => Program[I, B]
  ) extends Program[I, B]

// A way to inspect the next instruction of a program
enum ProgramView[I[_], A]:
  case Return[I[_], A](value: A) extends ProgramView[I, A]
  case Then[I[_], A, B](
    instruction: I[A],
    continuation: A => Program[I, B]
  ) extends ProgramView[I, B]

object Program:
  def fromValue[I[_], A](value: A): Program[I, A] = Return(value)
  def fromInstruction[I[_], A](instruction: I[A]) =
    Instruction(instruction)

  // format: off
  def inject[I[_], A, I2[_]](instruction: I[A])(using T: I ~> I2) =
    Program.fromInstruction(T(instruction))
  // format: on

  given programIsMonad[I[_]]: Monad[Program[I, _]] with
    def pure[A](a: A): Program[I, A] = Return(a)
    extension [A](program: Program[I, A])
      override def flatMap[B](
        continuation: A => Program[I, B]
      ): Program[I, B] = Then(program, continuation)

  extension [I[_], A](program: Program[I, A])
    def interpret[M[_]: Monad](interpreter: I ~> M): M[A] =
      program match
        case Return(value)            => Monad.pure(value)
        case Instruction(instruction) => interpreter(instruction)
        case Then(program, continuation) =>
          for
            a <- program.interpret(interpreter)
            continuationProgram = continuation(a)
            result <- continuationProgram.interpret(interpreter)
          yield result

    def andThen[B](continuation: A => Program[I, B]) =
      Then(program, continuation)

    @tailrec def next: ProgramView[I, A] = program match
      case Return(value) => ProgramView.Return(value)
      case Instruction(instruction) =>
        ProgramView.Then(instruction, Return(_))
      case Then(program, f) =>
        program match
          case Instruction(instruction) =>
            ProgramView.Then(instruction, f)
          case Return(value) => f(value).next
          case Then(program, g) =>
            program.andThen(x => g(x).andThen(f)).next

  def empty[I[_]]: Program[I, Unit] = Monad.pure(())
