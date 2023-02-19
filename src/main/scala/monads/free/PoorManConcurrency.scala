package monads.free

import monads.free.lib.*
import monads.Monad.{*, given}

object Concurrency:
  import ConcurrentDSL.*

  type Concurrent[A] = Program[ConcurrentDSL, A]
  enum ConcurrentDSL[A]:
    case YieldControl extends ConcurrentDSL[Unit]
    case Stop extends ConcurrentDSL[Nothing]
    case Perform[A](action: () => A) extends ConcurrentDSL[A]
    case Fork(process: Concurrent[Unit])
        extends ConcurrentDSL[Unit]

  def yieldControl = Program.fromInstruction(YieldControl)
  def stop = Program.fromInstruction(Stop)
  def perform[A](action: => A) =
    Program.fromInstruction(Perform(() => action))
  def fork(program: Concurrent[Unit]) =
    Program.fromInstruction(Fork(program))

  // format: off
  extension (thread: Concurrent[Unit])
    def runSingleThreadedCooperative: Unit =
      runThreads(List(thread))

    private def runThreads(threads: List[Concurrent[Unit]]): Unit =
      threads match
        case Nil => ()
        case thread :: threads => 
          runInstruction(thread.next, threads)
  // format: on

    private def runInstruction(
      instruction: ProgramView[ConcurrentDSL, Unit],
      threads: List[Concurrent[Unit]]
    ): Unit =
      instruction match
        case ProgramView.Return(_) => runThreads(threads)
        case ProgramView.Then(instruction, continuation) =>
          instruction match
            case Perform(action) =>
              val result = action()
              val newThreads = continuation(result) +: threads
              runThreads(newThreads)
            case Stop =>
              runThreads(threads)
            case YieldControl =>
              val newThreads = threads :+ continuation(())
              runThreads(newThreads)
            case Fork(process) =>
              val newThreads =
                continuation(()) +: threads :+ process
              runThreads(newThreads)

  object Examples:
    val program =
      perform(println("forking"))
        >> fork(thread("1", "hello world"))
        >> yieldControl
        >> perform(println("thread1 - ending"))

    def thread(name: String, message: String) =
      perform(println(f"thread $name - $message"))
        >> yieldControl
        >> perform(println(f"thread $name - ending"))

    @main def concurrent =
      program.runSingleThreadedCooperative
