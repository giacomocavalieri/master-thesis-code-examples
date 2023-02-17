package monads.free

import monads.Monad.{given, *}
import monads.mtl.{User, UserId}
import monads.IO
import monads.free.lib.*

enum UserStoreDSL[A]:
  case Get(userId: UserId) extends UserStoreDSL[Option[User]]
  case Save(user: User) extends UserStoreDSL[Unit]
  case Delete(userId: UserId) extends UserStoreDSL[Unit]

type UserStore[A] = Program[UserStoreDSL, A]

object UserStore:
  import UserStoreDSL.*

  def get[I[_]: With[UserStoreDSL]](userId: UserId) =
    Program.injectInstruction(Get(userId))

  def save[I[_]: With[UserStoreDSL]](user: User) =
    Program.injectInstruction(Save(user))

  def delete[I[_]: With[UserStoreDSL]](userId: UserId) =
    Program.injectInstruction(Delete(userId))

  object Examples:
    def program =
      for user <- UserStore.get[UserStoreDSL](UserId(1))
      yield 10

    val res = program.interpret(dbInterpreter)

    def dbInterpreter = new (UserStoreDSL ~> IO):
      def apply[A](term: UserStoreDSL[A]): IO[A] =
        term match
          case Get(userId)    => IO(???)
          case Save(user)     => IO(???)
          case Delete(userId) => IO(???)
