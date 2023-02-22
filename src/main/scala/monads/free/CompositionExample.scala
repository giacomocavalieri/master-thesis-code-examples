package monads.free

import monads.free.lib.{Program, With, ~>, :|}
import monads.Monad.{*, given}
import monads.IO
import monads.free.lib.{Interpreter, ProgramView}
import scala.annotation.tailrec

enum LogLevel:
  case Error, Warning, Info

enum LogDSL[A]:
  case Log(logLevel: LogLevel, msg: String) extends LogDSL[Unit]

object Log:
  def log[I[_]: With[LogDSL]](logLevel: LogLevel, msg: String) =
    Program.inject(LogDSL.Log(logLevel, msg))

  val ioInterpreter = new (LogDSL ~> IO):
    def apply[A](dsl: LogDSL[A]) = dsl match
      case LogDSL.Log(logLevel, msg) =>
        IO.putStrLn(f"[$logLevel] $msg")

enum ConsoleDSL[A]:
  case GetLine() extends ConsoleDSL[String]
  case PrintLine(msg: String) extends ConsoleDSL[Unit]

object Console:
  def getLine[I[_]: With[ConsoleDSL]] =
    Program.inject(ConsoleDSL.GetLine())

  def printLine[I[_]: With[ConsoleDSL]](msg: String) =
    Program.inject(ConsoleDSL.PrintLine(msg))

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
  val interpreter =
    Log.ioInterpreter.combinedWith(Console.ioInterpreter)
  echo.interpret(interpreter).unsafeRun()

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
