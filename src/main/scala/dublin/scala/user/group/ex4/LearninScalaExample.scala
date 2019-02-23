package dublin.scala.user.group.ex4

import cats.data.{State, StateT}
import cats.effect.IO
import cats.{Eval, arrow}
import dublin.scala.user.group.model._

object LearninScalaExample {
  def main(args: Array[String]): Unit = {
    learnScala().run(CreditCard(2000)).value
    learnScalaIO().run(CreditCard(2000)).unsafeRunSync()
  }

  def buyCourseraSubscription() = State[CreditCard, Item](creditCard =>
    creditCard.copy(balance = creditCard.balance - ScalaCourse.price) -> ScalaCourse
  )

  def buyRedBook() = State[CreditCard, Item](creditCard =>
    creditCard.copy(balance = creditCard.balance - RedBook.price) -> RedBook
  )

  def buyScalaDaysTickets() = State[CreditCard, Item](creditCard =>
    creditCard.copy(balance = creditCard.balance - ScalaDaysTickets.price) -> ScalaDaysTickets
  )

  private def postTweet(items: List[Item], creditCard: CreditCard): Unit =
    println(s"Check this out ${items.mkString(",")}. And I have ${creditCard.balance} left on my account")

  def postTweetIO(items: List[Item], creditCard: CreditCard): IO[Unit] =
    IO(println(s"Check this out ${items.mkString(",")}. And I have ${creditCard.balance} left on my account"))

  def learnScala(): State[CreditCard, Unit] =
    for {
      scalaCourse <- buyCourseraSubscription()
      redBook <- buyRedBook()
      scalaDaysTickets <- buyScalaDaysTickets()
      creditCard <- State.get
    } yield postTweet(List(scalaCourse,
      redBook,
      scalaDaysTickets), creditCard)

  val evalToIO = new arrow.FunctionK[Eval, IO] {
    override def apply[A](fa: Eval[A]): IO[A] = IO(fa.value)
  }

  def learnScalaIO(): StateT[IO, CreditCard, Unit] =
    for {
      scalaCourse <- buyCourseraSubscription().mapK(evalToIO)
      redBook <- buyRedBook().mapK(evalToIO)
      scalaDaysTickets <- buyScalaDaysTickets().mapK(evalToIO)
      creditCard <- StateT.get[IO, CreditCard]
      _ <- StateT.liftF[IO, CreditCard, Unit](postTweetIO(List(scalaCourse,
        redBook,
        scalaDaysTickets), creditCard))
    } yield ()

}
