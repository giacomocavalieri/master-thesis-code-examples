package monads.free.lib

final case class LeftF[F[_], A](f: F[A])
final case class RightF[F[_], A](f: F[A])
type :|[F[_], G[_]] = [A] =>> LeftF[F, A] | RightF[G, A]

type :>>[F[_], G[_]] = Inject[F[_], G[_]]
type With[F[_]] = [G[_]] =>> F :>> G
type InjectibleIn[G[_]] = [F[_]] =>> F :>> G

trait Inject[F[_], G[_]]:
  def inject[A](f: F[A]): G[A]

object Inject:
  extension [F[_], A](f: F[A])
    def inject[G[_]](using I: F :>> G): G[A] = I.inject(f)

  given injectInItself[F[_]]: (F :>> F) with
    def inject[A](f: F[A]): F[A] = f

  given injectRightWithItself[F[_], G[_]]: (F :>> (G :| F)) with
    def inject[A](f: F[A]): (G :| F)[A] = RightF(f)

  given injectInProduct[F[_]: InjectibleIn[G], G[_], H[_]]
    : (F :>> (G :| H)) with
    def inject[A](f: F[A]): (G :| H)[A] = LeftF(f.inject)
