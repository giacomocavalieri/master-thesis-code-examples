package monads

import monads.Monad.{*, given}

final case class Parser[A](parse: String => Option[(A, String)])

object Parser:
  given Monad[Parser[_]] with
    def pure[A](a: A): Parser[A] = Parser(s => Some(a, s))
    extension [A](p: Parser[A])
      def flatMap[B](f: A => Parser[B]): Parser[B] =
        Parser(string0 =>
          p.parse(string0) match
            case None => None
            case Some((result, string1)) =>
              f(result).parse(string1)
        )

  def fail[A]: Parser[A] = Parser(_ => None)

  def char: Parser[Char] =
    Parser(string => string.headOption.map((_, string.tail)))

  def consumeN(n: Int): Parser[List[Char]] =
    List.fill(n)(char).sequence

  object Examples:
    def consumeTwo: Parser[(Char, Char)] =
      for
        c1 <- char
        c2 <- char
      yield (c1, c2)
