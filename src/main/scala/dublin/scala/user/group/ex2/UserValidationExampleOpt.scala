package dublin.scala.user.group.ex2

import dublin.scala.user.group.model.User

object UserValidationExampleOpt {
  def validateUser(userToValidate: User): Option[User] = for {
    _ <- validateAge(userToValidate)
    _ <- validatePassword(userToValidate)
  } yield userToValidate

  private def validateAge(user: User): Option[User] =
    if(user.age >= 18) Some(user) else None

  private def validatePassword(user: User): Option[User] =
    if(user.password.length >= 6) Some(user) else None
}
