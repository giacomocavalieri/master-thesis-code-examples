package monads.free.lib

import monads.Functor
import monads.Monad
import monads.Monad.{*, given}
import monads.free.lib.Inject.inject

type ~>[F[_], G[_]] = Interpreter[F, G]
trait Interpreter[F[_], G[_]]:
  def apply[A](f: F[A]): G[A]

extension [F[_], M[_]](interpreter: F ~> M)
  def interpret[A](program: Program[F, A]): Monad[M] ?=> M[A] =
    program.interpret(interpreter)

  def or[G[_]](gToM: G ~> M): (F :| G) ~> M =
    new ((F :| G) ~> M):
      def apply[A](dsl: (F :| G)[A]): M[A] =
        dsl match
          case LeftF(f)  => interpreter(f)
          case RightF(g) => gToM(g)

// Program is a "Free Operational Monad"
enum Program[I[_], A]:
  case Instruction[I[_], A](instruction: I[A])
      extends Program[I, A]
  case Return[I[_], A](value: A) extends Program[I, A]
  case Then[I[_], A, B](
    program: Program[I, A],
    f: A => Program[I, B]
  ) extends Program[I, B]

object Program:
  def fromValue[I[_], A](value: A): Program[I, A] = Return(value)
  def fromInstruction[I[_], A](instruction: I[A]) =
    Instruction(instruction)

  def injectInstruction[I[_]: InjectibleIn[I2], A, I2[_]](
    instruction: I[A]
  ) = Program.fromInstruction(instruction).injectProgram

  given termIsMonad[I[_]]: Monad[Program[I, _]] with
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

    def injectProgram[I2[_]]: (I :>> I2) ?=> Program[I2, A] =
      val interpreter = new (I ~> Program[I2, _]):
        def apply[A](instruction: I[A]): Program[I2, A] =
          Program.fromInstruction(instruction.inject)
      program.interpret(interpreter)
