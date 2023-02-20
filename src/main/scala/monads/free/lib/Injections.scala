package monads.free.lib

type :|[F[_], G[_]] = [A] =>> F[A] | G[A]

type With[F[_]] = [G[_]] =>> F InjectIn G
type InjectibleIn[G[_]] = [F[_]] =>> F InjectIn G

trait InjectIn[F[_], G[_]]:
  def inject[A](f: F[A]): G[A]

object InjectIn:
  extension [F[_], A](f: F[A])
    inline def inject[G[_]](using I: F InjectIn G): G[A] =
      I.inject(f)

  inline given right[F[_], G[_]]: InjectIn[F, F :| G] with
    inline def inject[A](f: F[A]) = f

  inline given leftProduct[F[_], G[_], H[_]](using
    F InjectIn G
  ): (F InjectIn (G :| H)) with
    inline def inject[A](f: F[A]) = f.inject
