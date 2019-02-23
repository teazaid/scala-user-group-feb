package dublin.scala.user.group.ex6

import cats.data.{StateT, WriterT}
import cats.effect.IO
import cats.implicits._
import dublin.scala.user.group.ex4.LearninScalaExample._
import dublin.scala.user.group.model.{CreditCard, Item}

object LearningScalaExample {
  def main(args: Array[String]): Unit = {

    val (endState, (log, _)) = learnScalaIO().run.run(CreditCard(2000)).unsafeRunSync()
    println(endState)
    println(log.mkString("\n"))
  }

  type SState[T] = StateT[IO, CreditCard, T]

  def learnScalaIO(): WriterT[SState, List[String], Unit] =
    for {
      scalaCourse <- WriterT.liftF[SState, List[String], Item](buyCourseraSubscription().mapK(evalToIO))
      _ <- WriterT.tell[SState, List[String]](List("Got Scala Course"))
      redBook <- WriterT.liftF[SState, List[String], Item](buyRedBook().mapK(evalToIO))
      _ <- WriterT.tell[SState, List[String]](List("Bought Red Book"))
      scalaDaysTickets <- WriterT.liftF[SState, List[String], Item](buyScalaDaysTickets().mapK(evalToIO))
      _ <- WriterT.tell[SState, List[String]](List("Finally going to Scala Days"))
      creditCard <- WriterT.liftF[SState, List[String], CreditCard](StateT.get[IO, CreditCard])
      _ <- WriterT.liftF[SState, List[String], Unit](StateT.liftF[IO, CreditCard, Unit](postTweetIO(List(scalaCourse,
        redBook,
        scalaDaysTickets), creditCard)))
    } yield ()

}


