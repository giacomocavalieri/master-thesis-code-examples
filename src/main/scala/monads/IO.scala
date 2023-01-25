package monads

import scala.util.{Try, Success, Failure}
import monads.Monad.{*, given}
import requests.*

final case class IO[A](unsafeRun: () => A)

object IO:
  given Monad[IO] with
    def pure[A](a: A): IO[A] = IO(() => a)
    extension [A](m: IO[A])
      def flatMap[B](f: A => IO[B]): IO[B] =
        IO(() => f(m.unsafeRun()).unsafeRun())

  def putStrLn(s: String): IO[Unit] = IO(() => println(s))
  def getLine: IO[String] = IO(() => scala.io.StdIn.readLine())

  object Examples:
    def urlProgram: IO[Unit] =
      def urlToResource(url: String): IO[Try[Response]] =
        IO(() => Try(requests.get(url)))

      def shouldRetry(response: Try[Response]): Boolean =
        response match
          case Failure(exception: RequestFailedException) =>
            val statusCode = exception.response.statusCode
            List(500, 503).contains(statusCode)
          case _ => false

      val times = 10
      val step = for
        _      <- putStrLn("URL of the resource: ")
        url    <- getLine
        result <- urlToResource(url).retry(times, shouldRetry)
        _ <- result match
          case None    => putStrLn("Failed after 10 retries")
          case Some(_) => putStrLn("Got a response")
      yield ()
      step.forever
