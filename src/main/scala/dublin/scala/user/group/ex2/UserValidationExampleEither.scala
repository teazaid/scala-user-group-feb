package dublin.scala.user.group.ex2

import dublin.scala.user.group.model.User

object UserValidationExampleEither {
  def validateUser(userToValidate: User): Either[String, User] = for {
    _ <- validateAge(userToValidate)
    _ <- validatePassword(userToValidate)
  } yield userToValidate

  private def validateAge(user: User): Either[String, User] =
    if(user.age >= 18) Right(user) else Left("Age is not valid")

  private def validatePassword(user: User): Either[String, User] =
    if(user.password.length >= 6) Right(user) else Left("Password is not valid")
}
