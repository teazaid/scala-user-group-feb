package dublin.scala.user.group.ex5

import cats.Monad
import cats.data.{State, StateT}
import cats.effect.IO
import cats.mtl.MonadState
import cats.mtl.implicits._
import cats.implicits._
import dublin.scala.user.group.model._

object LearninScalaFTExample {
  def main(args: Array[String]): Unit = {
    val scalaLearner = new RoadToMonads[StateT[IO, CreditCard, ?]]()
    scalaLearner.learnScala().run(CreditCard(2000)).unsafeRunSync()
  }

  type CCState[F[_]] = MonadState[F, CreditCard]

  class RoadToMonads[F[_] : CCState : Monad]() {
    val ms = implicitly[CCState[F]]
    val m = implicitly[Monad[F]]

    private def buyCourseraSubscription(): F[Item] =
      ms.modify(cc => cc.copy(balance = cc.balance - ScalaCourse.price))
        .map(_ => ScalaCourse)

    private def buyRedBook(): F[Item] =
      ms.modify(cc => cc.copy(balance = cc.balance - RedBook.price))
        .map(_ => RedBook)

    private def buyScalaDaysTickets(): F[Item] =
      ms.modify(cc => cc.copy(balance = cc.balance - ScalaDaysTickets.price))
        .map(_ => ScalaDaysTickets)

    private def postTweet(items: List[Item], creditCard: CreditCard): F[Unit] =
      m.pure(println(s"Check this out ${items.mkString(",")}. And I have ${creditCard.balance} left on my account"))

    def learnScala(): F[Unit] =
      for {
        scalaCourse <- buyCourseraSubscription()
        redBook <- buyRedBook()
        scalaDaysTickets <- buyScalaDaysTickets()
        creditCard <- ms.get
      } yield postTweet(List(scalaCourse,
        redBook,
        scalaDaysTickets), creditCard)
  }

}

