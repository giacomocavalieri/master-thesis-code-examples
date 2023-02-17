package monads.free
import monads.free.lib.{
  Program,
  With,
  ~>,
  or,
  :|,
  interpret,
  InjectibleIn
}
import monads.Monad.{*, given}
import monads.IO

enum LogLevel:
  case Error
  case Warning
  case Info

enum LogDSL[A]:
  case Log(logLevel: LogLevel, msg: String) extends LogDSL[Unit]

object Log:
  def log[I[_]: With[LogDSL]](logLevel: LogLevel, msg: String) =
    Program.injectInstruction(LogDSL.Log(logLevel, msg))

  def consoleInterpreter[I[_]: With[ConsoleDSL]] =
    new (LogDSL ~> Program[I, _]):
      def apply[A](dsl: LogDSL[A]) = dsl match
        case LogDSL.Log(logLevel, msg) =>
          Console.printLine(f"[$logLevel] $msg")

  val ioInterpreter = new (LogDSL ~> IO):
    def apply[A](dsl: LogDSL[A]) = dsl match
      case LogDSL.Log(logLevel, msg) =>
        IO.putStrLn(f"[$logLevel] $msg")

enum ConsoleDSL[A]:
  case GetLine() extends ConsoleDSL[String]
  case PrintLine(msg: String) extends ConsoleDSL[Unit]

object Console:
  def getLine[I[_]: With[ConsoleDSL]] =
    Program.injectInstruction(ConsoleDSL.GetLine())

  def printLine[I[_]: With[ConsoleDSL]](msg: String) =
    Program.injectInstruction(ConsoleDSL.PrintLine(msg))

  val ioInterpreter = new (ConsoleDSL ~> IO):
    def apply[A](dsl: ConsoleDSL[A]) = dsl match
      case ConsoleDSL.GetLine()      => IO.getLine
      case ConsoleDSL.PrintLine(msg) => IO.putStrLn(msg)

// format: off
def program[I[_]
    : With[ConsoleDSL]
    : With[LogDSL]
    : With[FailDSL]] =
  val step = for
    line <- Console.getLine
    _    <- Console.printLine(line)
    _    <- Log.log(LogLevel.Info, f"echoed: $line")
    _ <- when(line == "exit") {
      for
        _ <- Log.log(LogLevel.Info, "exited app")
        _ <- Fail.fail
      yield ()
    }
  yield ()
  step.forever
// format: on

@main def main =
  Console.ioInterpreter
    .or(Log.ioInterpreter)
    .or(Fail.ioInterpreter)
    .interpret(program)
    .unsafeRun()
