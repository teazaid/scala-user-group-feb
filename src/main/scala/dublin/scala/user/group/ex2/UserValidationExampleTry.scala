package dublin.scala.user.group.ex2

import dublin.scala.user.group.model.User

import scala.util.{Failure, Success, Try}

object UserValidationExampleTry {
  def validateUser(userToValidate: User): Try[User] = for {
    _ <- validateAge(userToValidate)
    _ <- validatePassword(userToValidate)
  } yield userToValidate

  private def validateAge(user: User): Try[User] =
    if(user.age >= 18) Success(user) else Failure(new Exception("Age is not valid"))

  private def validatePassword(user: User): Try[User] =
    if(user.password.length >= 6) Success(user) else Failure(new Exception("Password is not valid"))
}
