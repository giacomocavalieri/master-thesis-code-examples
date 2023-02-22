package monads.free

import monads.Monad.{given, *}
import monads.mtl.{User, UserId}
import monads.IO
import monads.free.lib.*

import UserStoreDSL.*
enum UserStoreDSL[A]:
  case Get(userId: UserId) extends UserStoreDSL[Option[User]]
  case Save(user: User) extends UserStoreDSL[Unit]
  case Delete(userId: UserId) extends UserStoreDSL[Unit]

object UserStore:
  def get[I[_]: With[UserStoreDSL]](userId: UserId) =
    Program.inject(Get(userId))

  def save[I[_]: With[UserStoreDSL]](user: User) =
    Program.inject(Save(user))

  def delete[I[_]: With[UserStoreDSL]](userId: UserId) =
    Program.inject(Delete(userId))

  object Examples:
    import scala.annotation.tailrec
    import monads.Monad
    import monads.mtl.Design.Examples.Production.*

    object UpdateAgeExample:
      import monads.mtl.Design.Examples.Delete

      // format: off
      def updateOrDelete[I[_]: With[UserStoreDSL]]
        (user: User)(f: User => User | Delete): Program[I, Unit] =
        f(user) match
          case updatedUser: User => UserStore.save(updatedUser)
          case _: Delete         => UserStore.delete(user.id)

      def updateOrDelete[I[_]: With[UserStoreDSL]]
        (userId: UserId)(f: User => User | Delete): Program[I, Unit] =
        UserStore.get(userId).flatMap {
          case Some(user) => updateOrDelete(user)(f)
          case None       => Program.empty
        }
      // format: on

      def updateAge[I[_]: With[UserStoreDSL]](userId: UserId) =
        updateOrDelete(userId) { user =>
          if user.age < 18
          then Delete
          else user.copy(age = user.age + 1)
        }

    import UpdateAgeExample.updateAge

    extension [A](program: Program[UserStoreDSL, A])
      @tailrec
      def runInProduction(dbConnection: DatabaseConnection): A =
        program.next match
          case ProgramView.Return(value) => value
          case ProgramView.Then(instruction, continuation) =>
            instruction match
              case Get(userId) =>
                // Actually fetch the user via the DB connection
                val user = ???
                continuation(user).runInProduction(dbConnection)
              case Save(user) =>
                // Actually save the user via the DB connection
                continuation(()).runInProduction(dbConnection)
              case Delete(userId) =>
                // Actually delete the user via the DB connection
                continuation(()).runInProduction(dbConnection)

    // format: off
    extension [A](program: Program[UserStoreDSL, A])
      @tailrec
      def runMocked(users: Map[UserId, User]): Map[UserId, User] =
        program.next match
          case ProgramView.Return(value) => users
          case ProgramView.Then(instruction, continuation) =>
            instruction match
              case Get(userId) =>
                continuation(users.get(userId)).runMocked(users)
              case Save(user) =>
                val updatedUsers = users + ((user.id, user))
                continuation(()).runMocked(updatedUsers)
              case Delete(userId) =>
                continuation(()).runMocked(users - userId)

    def testUpdateAgeDeletesUnderageUsers: Unit =
      val user = User(UserId(1), "Giacomo", 12)
      val users = Map(user.id -> user)
      val finalUsers = updateAge(user.id).runMocked(users)
      assert(finalUsers.isEmpty)
