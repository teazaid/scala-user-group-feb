package dublin.scala.user.group.ex2

import cats.MonadError
import cats.implicits._
import dublin.scala.user.group.model.User

import scala.annotation.tailrec
import scala.util.Try


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

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(a1)) => tailRecM(a1)(f)
      case Some(Right(b)) => Some(b)
    }
  }

  type F[T] = Try[T]
  type E = String

  implicit val mE = new MonadError[F, E] {
    override def raiseError[A](e: E): F[A] = ???

    override def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A] = ???

    override def pure[A](x: A): F[A] = ???

    override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = ???

    override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = ???
  }

  def main(args: Array[String]): Unit = {
    val validatorOpt = new Validator[Option]()
    validatorOpt.validateUser(User("user1", 0, "strong"))

    val validatorEither = new Validator[Either[String, ?]]()
    validatorEither.validateUser(User("user2", 18, "weak"))
  }

  type ValidationError[F[_]] = MonadError[F, String]

  class Validator[F[_]: ValidationError] {
    val mE = implicitly[ValidationError[F]]

    def validateUser(userToValidate: User)(): F[User] = for {
      _ <- validateAge(userToValidate)
      _ <- validatePassword(userToValidate)
    } yield userToValidate

    private def validateAge(user: User): F[User] =
      if (user.age >= 18) mE.pure(user) else mE.raiseError("Age is not valid")

    private def validatePassword(user: User): F[User] =
      if (user.password.length >= 6) mE.pure(user) else mE.raiseError("Password is not valid")
  }

}
