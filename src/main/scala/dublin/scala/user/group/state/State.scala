package dublin.scala.user.group.state

case class State[S, A](run: S => (S, A)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B] { s0 =>
    val (s1, a) = run(s0)
    f(a).run(s1)
  }

  def map[B](f: A => B): State[S, B] = State[S, B] { s0 =>
    val (s1, a) = run(s0)
    s1 -> f(a)
  }
}
