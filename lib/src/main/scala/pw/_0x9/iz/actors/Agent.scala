package pw._0x9.iz.actors

import akka.actor._
import pw._0x9.iz._

sealed trait AgentMessage
case class BestMoves(s: GameState, config: Config) extends AgentMessage

class AgentActor(stageActor: ActorRef) extends Actor {
  private[this] val agent = new Agent

  def receive: Receive = {
    case BestMoves(s, config) =>
      agent.bestMoves(s, config.maxThinkTime) match {
        case Seq(Tick)     => // do nothing
        case Seq(HardDrop) => config.onDrop foreach { stageActor ! _ }
        case ms            =>
          ms foreach {
            case Tick | HardDrop => // do nothing
            case m =>
              stageActor ! m
              Thread.sleep(config.minActionTime)
          }
      }
      sender ! ()
  }
}
