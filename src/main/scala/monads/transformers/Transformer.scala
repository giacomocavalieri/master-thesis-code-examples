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

  object Associativity:
    import monads.IO

    type Program1[A] = StateT[String, OptionT[IO, _], A]
    type Program2[A] = OptionT[StateT[String, IO, _], A]

    val program1: Program1[Int] = ???
    val result1: Option[(Int, String)] =
      program1.runStateT("initial state").runOptionT.unsafeRun()

    val program2: Program2[Int] = ???
    val result2: (Option[Int], String) =
      program2.runOptionT.runStateT("initial state").unsafeRun()

  object Parser:
    import monads.State

    opaque type Parser[A] = OptionT[State[String, _], A]
    extension [A](parser: Parser[A])
      def parse(string: String): (Option[A], String) =
        parser.runOptionT.runState(string)

    def fail[A]: Parser[A] = OptionT.fail
    def get: Parser[String] = State.get.lift[OptionT]
    def set(string: String): Parser[Unit] =
      State.set(string).lift[OptionT]

    def char: Parser[Char] =
      for
        string <- get
        result <- string.headOption match
          case Some(char) =>
            for _ <- set(string.tail)
            yield char
          case None => fail
      yield result
