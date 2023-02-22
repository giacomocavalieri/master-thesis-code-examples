package monads.free.lib
import monads.Monad

trait Interpreter[F[_], G[_]]:
  def apply[A](f: F[A]): G[A]

type ~>[F[_], G[_]] = Interpreter[F, G]
type With[F[_]] = [G[_]] =>> F ~> G

type :|[F[_], G[_]] = [A] =>> F[A] | G[A]

object Interpreter:
  given left[F[_], G[_]]: (F ~> (F :| G)) with
    def apply[A](f: F[A]) = f

  given leftProduct[F[_], G[_], H[_]](using
    T: F ~> G
  ): (F ~> (G :| H)) with
    def apply[A](f: F[A]) = T(f)

  extension [F[_], M[_]](interpreter: F ~> M)
    inline def combinedWith[G[_]](gToM: G ~> M): (F :| G) ~> M =
      new ((F :| G) ~> M):
        def apply[A](dsl: (F :| G)[A]): M[A] =
          dsl match
            case f: F[A] => interpreter(f)
            case g: G[A] => gToM(g)
