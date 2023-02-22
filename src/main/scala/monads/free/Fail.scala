package monads.free

import monads.Monad
import monads.free.lib.*
import monads.IO

enum FailDSL[A]:
  case Fail[A]() extends FailDSL[A]

object Fail:
  def fail[A, I[_]: With[FailDSL]] =
    Program.inject(FailDSL.Fail())

  val optionInterpreter = new (FailDSL ~> Option):
    def apply[A](f: FailDSL[A]): Option[A] = f match
      case FailDSL.Fail() => None

  val ioInterpreter = new (FailDSL ~> IO):
    def apply[A](f: FailDSL[A]): IO[A] = f match
      case FailDSL.Fail() =>
        IO(() => throw Exception("fail called"))
