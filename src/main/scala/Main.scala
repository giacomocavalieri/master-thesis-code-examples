import monads.*
import monads.transformers.OptionT

@main def main =
  OptionT.Examples.failAndIO.runOptionT.unsafeRun()
  // println(Parser.consumeN(2).parse("abc"))
