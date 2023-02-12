package monads

final case class Identity[A](a: A)

object Identity:
  given Monad[Identity] with
    def pure[A](a: A): Identity[A] = Identity(a)

    extension [A](m: Identity[A])
      def flatMap[B](f: A => Identity[B]): Identity[B] = f(m.a)

  given identityToValue[A]: Conversion[Identity[A], A] with
    override def apply(x: Identity[A]): A = x.a
