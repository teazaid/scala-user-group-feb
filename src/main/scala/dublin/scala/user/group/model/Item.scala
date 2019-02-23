package dublin.scala.user.group.model

sealed abstract class Item(val price: Long)

case object ScalaCourse extends Item(100)
case object RedBook extends Item(200)
case object ScalaDaysTickets extends Item(1000)