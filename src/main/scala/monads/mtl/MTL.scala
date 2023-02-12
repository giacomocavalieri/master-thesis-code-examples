package monads.mtl

import monads.IO
import monads.Monad
import monads.Monad.{given, *}
import monads.transformers.{StateT, OptionT, given, *}
import monads.transformers.StateT.StateTFixS
import monads.transformers.MonadTransformer
import scala.util.Random

object TransformersProblemsExamples:
  def manualLifting: StateT[Int, OptionT[IO, _], String] =
    for
      _ <- IO.putStrLn("asd").lift[OptionT].lift[StateTFixS[Int]]
      _ <- OptionT.fail[IO, Any].lift[StateTFixS[Int]]
    yield "asd"

  def manualLifting2: OptionT[IO, String] =
    for
      _ <- IO.putStrLn("test").lift[OptionT]
      _ <- OptionT.fail[IO, Any]
    yield "result"

  def manualLifting3: OptionT[StateT[Int, IO, _], String] =
    for
      _ <- IO.putStrLn("asd").lift[StateTFixS[Int]].lift[OptionT]
      _ <- OptionT.fail[StateT[Int, IO, _], Any]
    yield "result"

  def example[M[_]: Monad: Fail: HasState[Int]]: M[Int] =
    for
      state <- State[M, Int].get
      newState = state - 1
      _ <-
        if state == 0
        then Fail[M].fail
        else State[M, Int].set(newState)
    yield newState

  object LeastPrivilege:
    class CSV:
      def getIntColumn(column: String): App[List[Int]] = ???

    def readFile(file: String): App[String] = ???
    def parseCSV(rawData: String): App[CSV] = ???

    type App = OptionT[IO, _]
    def mainAction: App[Int] = for
      rawData <- readFile("data.csv")
      csv     <- parseCSV(rawData)
      column  <- csv.getIntColumn("column")
    yield column.sum

  @main def runExample =
    import monads.transformers.{OptionT, StateT, given}
    import monads.Identity

    type Stack = OptionT[StateT[Int, Identity, _], _]
    type IOBasedStack = StateT[Int, IO, _]

    println(example[Stack].runOptionT.runStateT(0))
    println(example[IOBasedStack].runStateT(0).unsafeRun())

object MTL:
  object Examples:
    import Fail.Examples.divide

    def effects[M[_]: HasState[Int]: Fail: Monad]: M[String] =
      for
        state    <- State[M, Int].get
        newState <- divide(10, state)
        _        <- State[M, Int].set(newState)
      yield f"The result is $newState"

    import monads.Identity

    def interpretEffects: Unit =
      type Stack1 = OptionT[StateT[Int, Identity, _], _]
      val res1: (Option[String], Int) =
        effects[Stack1].runOptionT.runStateT(1)

      type Stack2 = StateT[Int, OptionT[Identity, _], _]
      val res2: Option[(String, Int)] =
        effects[Stack2].runStateT(1).runOptionT

      type Stack3 = StateT[Int, IO, _]
      val res3: (String, Int) =
        effects[Stack3].runStateT(1).unsafeRun()
