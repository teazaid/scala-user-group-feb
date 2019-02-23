package dublin.scala.user.group.ex2

import cats.MonadError
import cats.implicits._
import dublin.scala.user.group.model.User
import scala.util.Try

class Validator[F[_]](implicit mE: MonadError[F, String]) {

  def validateUser(userToValidate: User)(): F[User] = for {
    _ <- validateAge(userToValidate)
    _ <- validatePassword(userToValidate)
  } yield userToValidate

  private def validateAge(user: User): F[User] =
    if (user.age >= 18) mE.pure(user) else mE.raiseError("Age is not valid")

  private def validatePassword(user: User): F[User] =
    if (user.password.length >= 6) mE.pure(user) else mE.raiseError("Password is not valid")
}

object Runner {
  implicit val monadErrorOpt = new MonadError[Option, String] {
    override def raiseError[A](e: String): Option[A] = None

    override def handleErrorWith[A](fa: Option[A])(f: String => Option[A]): Option[A] =
      fa match {
        case r@Some(_) => r
        case None => None
      }

    override def pure[A](x: A): Option[A] = Some(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = for {
      a <- fa
      b <- f(a)
    } yield b

    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = flatMap(f(a)) {
      case Right(v) => Some(v)
      case Left(_) => tailRecM(a)(f)
    }
  }

  type Effect[T] = Try[T]
  type Error = String

  implicit val mE = new MonadError[Effect, Error] {
    override def raiseError[A](e: Error): Effect[A] = ???

    override def handleErrorWith[A](fa: Effect[A])(f: Error => Effect[A]): Effect[A] = ???

    override def pure[A](x: A): Effect[A] = ???

    override def flatMap[A, B](fa: Effect[A])(f: A => Effect[B]): Effect[B] = ???

    override def tailRecM[A, B](a: A)(f: A => Effect[Either[A, B]]): Effect[B] = ???
  }

  def main(args: Array[String]): Unit = {
    val validatorOpt = new Validator[Option]()
    validatorOpt.validateUser(User("user1", 0, "strong"))

    val validatorEither = new Validator[Either[String, ?]]()
    validatorEither.validateUser(User("user2", 18, "weak"))
  }
}
