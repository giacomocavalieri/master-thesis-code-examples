import monads.*
import monads.transfromers.OptionT

@main def main =
  OptionT.Examples.failAndIO.runOptionT.unsafeRun()
  // println(Parser.consumeN(2).parse("abc"))
