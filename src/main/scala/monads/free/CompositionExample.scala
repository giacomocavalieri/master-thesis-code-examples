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
import monads.free.lib.{Interpreter, ProgramView}
import monads.free.lib.InjectIn.inject
import scala.annotation.tailrec

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

def echo[I[_]: With[ConsoleDSL]: With[LogDSL]]
  : Program[I, Unit] =
  for
    line <- Console.getLine
    _ <-
      if (line == "quit")
      then Log.log(LogLevel.Info, "quitting")
      else
        Console.printLine(line)
          >> Log.log(LogLevel.Info, f"echoed a line")
          >> echo
  yield ()

@main def combiningInterpreters =
  val logToConsole =
    Log.consoleInterpreter.or(Interpreter.passThrough)

  echo // : Program[ConsoleDSL :| LogDSL]
    .interpret(logToConsole) // : Program[ConsoleDSL]
    .interpret(Console.ioInterpreter) // : IO[Unit]
    .unsafeRun() // : Unit

@main def manualInterpreter =
  interpret(echo)

  @tailrec def interpret[A](
    program: Program[LogDSL :| ConsoleDSL, A]
  ): A =
    program.next match
      case ProgramView.Return(a) => a
      case ProgramView.Then(instruction, continuation) =>
        instruction match
          case ConsoleDSL.PrintLine(msg) =>
            println(msg)
            interpret(continuation(()))
          case ConsoleDSL.GetLine() =>
            val line = scala.io.StdIn.readLine()
            interpret(continuation(line))
          case LogDSL.Log(logLevel, msg) =>
            println(f"[$logLevel] $msg")
            interpret(continuation(()))
