package dublin.scala.user.group.ex3

import dublin.scala.user.group.model._

object LearningScalaExample {
  def main(args: Array[String]): Unit = {
    learnScala(CreditCard(2000))
  }

  private def buyCourseraSubscription(creditCard: CreditCard): (CreditCard, Item) = {
    creditCard.copy(balance = creditCard.balance - ScalaCourse.price) -> ScalaCourse
  }

  private def buyRedBook(creditCard: CreditCard): (CreditCard, Item) = {
    creditCard.copy(balance = creditCard.balance - RedBook.price) -> RedBook
  }

  private def buyScalaDaysTickets(creditCard: CreditCard): (CreditCard, Item) = {
    creditCard.copy(balance = creditCard.balance - ScalaDaysTickets.price) -> ScalaDaysTickets
  }

  private def postTweet(items: List[Item], creditCard: CreditCard): Unit =
    println(s"Check this out ${items.mkString(",")}. And I have ${creditCard.balance} left on my account")

  def learnScala(creditCard: CreditCard): Unit = {
    val (creditCard1, courseraScalaCourse) = buyCourseraSubscription(creditCard)
    val (creditCard2, scalaRedBook) = buyRedBook(creditCard1)
    val (creditCard3, scaladaysTickets) = buyScalaDaysTickets(creditCard1)
    postTweet(List(courseraScalaCourse,
      scalaRedBook,
      scaladaysTickets), creditCard3)
  }
}
