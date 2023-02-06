package monads.transformers

import monads.Monad
import monads.Monad.{*, given}

trait MonadTransformer[T[_[_], _]]:
  def lift[M[_]: Monad, A](m: M[A]): T[M, A]

object Examples:
  final case class T[M[_], A]()

  given MonadTransformer[T] with
    def lift[M[_]: Monad, A](m: M[A]): T[M, A] = ???

  given transformerInstance[M[_]: Monad]: Monad[T[M, _]] with
    def pure[A](a: A): T[M, A] = ???
    extension [A](t: T[M, A])
      def flatMap[B](f: A => T[M, B]): T[M, B] = ???

  object Composition:
    import monads.transformers.StateT.StateTFixS
    import monads.IO

    type Program[A] = OptionT[StateT[String, IO, _], A]
    def program: Program[Int] =
      for
        s <- IO.getLine.lift[StateTFixS[String]].lift[OptionT]
        _ <-
          if s == "fail"
          then OptionT.fail: Program[Unit]
          else StateT.set[String, IO](s).lift[OptionT]
      yield s.length

    @main def runProgram: (Option[Int], String) =
      program.runOptionT
        .runStateT("initial state")
        .unsafeRun()
