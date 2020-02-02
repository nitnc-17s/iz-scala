package pw._0x9.iz.actors

import akka.actor._
import pw._0x9.iz._

sealed trait StateMessage
case object GetState extends StateMessage
case class SetState(s: GameState) extends StateMessage
case object GetView extends StateMessage

class StateActor(s0: GameState) extends Actor {
  private[this] val state: GameState = s0

  def receive: Receive = onMessage(state)

  private def onMessage(state: GameState): Receive = {
    case GetState => sender ! state
    case SetState(s) => context.become(onMessage(s))
    case GetView => sender ! state.view
  }
}
