package monads

final case class Identity[A](a: A)

object Identity:
  given Monad[Identity] with
    def pure[A](a: A): Identity[A] = Identity(a)

    extension [A](m: Identity[A])
      def flatMap[B](f: A => Identity[B]): Identity[B] = f(m.a)
