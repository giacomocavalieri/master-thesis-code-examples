package monads.free.lib

final case class LeftF[F[_], A](f: F[A])
final case class RightF[F[_], A](f: F[A])
type :|[F[_], G[_]] = [A] =>> LeftF[F, A] | RightF[G, A]

type With[F[_]] = [G[_]] =>> F InjectIn G
type InjectibleIn[G[_]] = [F[_]] =>> F InjectIn G

trait InjectIn[F[_], G[_]]:
  def inject[A](f: F[A]): G[A]

object InjectIn:
  extension [F[_], A](f: F[A])
    def inject[G[_]](using I: F InjectIn G): G[A] = I.inject(f)

  given identity[F[_]]: (F InjectIn F) with
    def inject[A](f: F[A]) = f

  given right[F[_], G[_]]: (F InjectIn (G :| F)) with
    def inject[A](f: F[A]) = RightF(f)

  given leftProduct[F[_]: InjectibleIn[G], G[_], H[_]]
    : (F InjectIn (G :| H)) with
    def inject[A](f: F[A]) = LeftF(f.inject)
