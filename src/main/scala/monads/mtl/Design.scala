package monads.mtl

import scala.compiletime.summonInline
import monads.{Identity, IO, Monad}
import monads.Monad.{given, *}
import monads.transformers.MonadTransformer
import monads.transformers.StateT
import monads.transformers.StateT.{given, *}
import monads.transformers.StateT.Examples.State

final case class UserId(id: Int)
final case class User(id: UserId, name: String, age: Int)

trait UserStore[M[_]]:
  def get(userId: UserId): M[Option[User]]
  def save(user: User): M[Unit]
  def delete(userId: UserId): M[Unit]

object UserStore:
  inline def apply[M[_]: UserStore] = summonInline[UserStore[M]]

object Design:
  object Examples:

    trait Delete
    object Delete extends Delete
      
    // format: off
    def updateOrDelete[M[_]: UserStore]
      (user: User)(f: User => User | Delete): M[Unit] =
      f(user) match
        case updatedUser: User => UserStore[M].save(updatedUser)
        case _: Delete         => UserStore[M].delete(user.id)

    def updateOrDelete[M[_]: Monad: UserStore]
      (userId: UserId)(f: User => User | Delete): M[Unit] =
      UserStore[M].get(userId).flatMap {
        case Some(user) => updateOrDelete(user)(f)
        case None       => Monad.pure(())
      }
    // format: on

    def updateAge[M[_]: Monad: UserStore](userId: UserId) =
      updateOrDelete(userId) { user =>
        if user.age < 18
        then Delete
        else user.copy(age = user.age + 1)
      }

    object Production:
      final case class DatabaseConnection()
      final case class Runtime(connection: DatabaseConnection)

      type ProductionRunner = StateT[Runtime, IO, _]
      def getRuntime: ProductionRunner[Runtime] = StateT.get

      given UserStore[ProductionRunner] with
        def get(userId: UserId) = for
          runtime <- getRuntime
          connection = runtime.connection
          // use the database connection to perform the SELECT query
          user <- ??? : ProductionRunner[Option[User]]
        yield user

        def save(user: User) = ???
        def delete(userId: UserId) = ???

    object Testing:
      type State[S, A] = StateT[S, Identity, A]
      object State:
        def get[S]: State[S, S] = StateT.get
        def set[S](s: S): State[S, Unit] = StateT.set(s)
        def update[S](f: S => S): State[S, Unit] =
          get.flatMap(s => set(f(s)))

      final case class Runtime(users: Map[UserId, User])
      type TestRunner = State[Runtime, _]

      def updateUsers(
        update: Map[UserId, User] => Map[UserId, User]
      ): TestRunner[Unit] =
        State.update { runtime =>
          val newUsers = update(runtime.users)
          runtime.copy(users = newUsers)
        }

      // format: off
      given UserStore[TestRunner] with
        def get(userId: UserId) = State.get.map(_.users.get(userId))
        def save(user: User) = updateUsers(_ + (user.id -> user))
        def delete(userId: UserId) = updateUsers(_ - userId)

      object Test:
        def testUpdateAgeUpdatesTheUser: Unit =
          val user = User(UserId(1), "Giacomo", 24)
          val runtime = Runtime(Map(user.id -> user))
          val finalRuntime = updateAge[TestRunner](user.id).runStateT(runtime)._2
          val finalUser = finalRuntime.users.get(user.id).get

          assert(finalRuntime.users.size == 1)
          assert(finalUser.id == user.id)
          assert(finalUser.name == user.name)
          assert(finalUser.age == user.age + 1)

        def testUpdateAgeDeletesUnderageUsers: Unit =
          val user = User(UserId(1), "Giacomo", 12)
          val runtime = Runtime(Map(user.id -> user))
          val finalRuntime =
            updateAge[TestRunner](user.id).runStateT(runtime)._2
          assert(finalRuntime.users.isEmpty)   
