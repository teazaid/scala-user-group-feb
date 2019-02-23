package dublin.scala.user.group.ex6

import java.time.LocalDateTime

import cats.Monad
import cats.data.{StateT, WriterT}
import cats.effect.IO
import cats.mtl.{FunctorTell, MonadState}
import cats.mtl.implicits._
import cats.implicits._
import dublin.scala.user.group.model._

object LearningScalaFTExample {
  def main(args: Array[String]): Unit = {

    type ST[T] = StateT[IO, CreditCard, T]
    type WR[T] = WriterT[ST, List[String], T]

    val scalaLearner = new ScalaLearner[WR]()
    val (endState, (log, _)) = scalaLearner.learnScala().run.run(CreditCard(2000)).unsafeRunSync()
  }

  type Logger[F[_]] = FunctorTell[F, List[String]]
  type CCState[F[_]] = MonadState[F, CreditCard]

  class ScalaLearner[F[_] : Logger : CCState : Monad]() {
    val ms = implicitly[CCState[F]]
    val m = implicitly[Monad[F]]
    val logger = implicitly[Logger[F]]

    private def buyCourseraSubscription(): F[Item] =
      ms.modify(cc => cc.copy(balance = cc.balance - ScalaCourse.price)).map(_ => ScalaCourse)

    private def buyRedBook(): F[Item] =
      ms.modify(cc => cc.copy(balance = cc.balance - RedBook.price)).map(_ => RedBook)

    private def buyScalaDaysTickets(): F[Item] =
      ms.modify(cc => cc.copy(balance = cc.balance - ScalaDaysTickets.price)).map(_ => ScalaDaysTickets)

    private def postTweet(items: List[Item], creditCard: CreditCard): F[Unit] =
      m.pure(println(s"Check this out ${items.mkString(",")}. And I have ${creditCard.balance} left on my account"))

    def learnScala(): F[Unit] =
      for {
        scalaCourse <- buyCourseraSubscription()
        _ <- logger.tell(List("Got Scala Course"))
        redBook <- buyRedBook()
        _ <- logger.tell(List("Bought Red Book"))
        scalaDaysTickets <- buyScalaDaysTickets()
        _ <- logger.tell(List("Finally going to Scala Days"))
        creditCard <- ms.get
      } yield postTweet(List(scalaCourse,
        redBook,
        scalaDaysTickets), creditCard)
  }

}
