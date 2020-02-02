package pw._0x9.iz.actors

import akka.actor._
import akka.pattern.ask
import pw._0x9.iz._

import scala.concurrent.Await
import scala.concurrent.duration._

sealed trait StageMessage
case object MoveLeft extends StageMessage
case object MoveRight extends StageMessage
case object RotateCW extends StageMessage
case object RotateCCW extends StageMessage
case object Hold extends StageMessage
case object Tick extends StageMessage
case object Drop extends StageMessage
case object Attack extends StageMessage

class StageActor(stateActor: ActorRef) extends Actor {
  import Stage._

  def receive: Receive = {
    case MoveLeft  => updateState {moveLeft}
    case MoveRight => updateState {moveRight}
    case RotateCW  => updateState {rotateCW}
    case RotateCCW => updateState {rotateCCW}
    case Hold      => updateState {hold}
    case Tick      => updateState {tick}
    case Drop      => updateState {drop}
    case Attack    => updateState {notifyAttack}
  }
  private[this] def opponent: ActorSelection =
    if (self.path.name == "stageActor1") context.actorSelection("/user/stageActor2")
    else context.actorSelection("/user/stageActor1")
  private[this] def updateState(trans: GameState => GameState) {
    val future = (stateActor ? GetState)(1 second).mapTo[GameState]
    val s1 = Await.result(future, 1 second)
    val s2 = trans(s1)
    stateActor ! SetState(s2)
    (0 to s2.lastDeleted - 2) foreach { i =>
      opponent ! Attack
    }
  }
}
