package dublin.scala.user.group.ex1

import dublin.scala.user.group.model.User

object UserValidationExample {

  def validateUser(userToValidate: User): User = {
    validateAge(userToValidate)
    validatePassword(userToValidate)
  }

  private def validateAge(user: User): User =
    if(user.age >= 18) user else throw new Exception("Age is not valid")

  private def validatePassword(user: User): User =
    if(user.password.length >= 6) user else throw new Exception("Password is not valid")
}
