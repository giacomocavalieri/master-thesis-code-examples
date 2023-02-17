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

  def singleThreaded[A](thread: Concurrent[Unit]): Unit =
    runThreads(List(thread))

  def runThreads(threads: List[Concurrent[Unit]]): Unit =
    threads match
      case thread :: threads => runSingle(thread.next, threads)
      case Nil               => ()

  def runSingle(
    thread: ProgramView[ConcurrentDSL, Unit],
    threads: List[Concurrent[Unit]]
  ): Unit =
    thread match
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
              threads :+ continuation(()) :+ process
            runThreads(newThreads)

  @main def concurrent =
    val concurrentProgram =
      for
        _ <- perform(println("1"))
        _ <- fork {
          for {
            _ <- perform(println("3"))
          } yield ()
        }
        _ <- yieldControl
        _ <- perform(println("2"))
      yield ()

    singleThreaded(concurrentProgram)
