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
  private[this] def updateState(trans: GameState => GameState): Unit = {
    val future = (stateActor ? GetState)(1 second).mapTo[GameState]
    val s1 = Await.result(future, 1 second)
    val s2 = trans(s1)
    stateActor ! SetState(s2)

    if (s2.hasNotYetAttacked) {
      val attackTimes = s2.lastClear match {
        case Single | MiniTSpinSingle => 0
        case Double | MiniTSpinDouble => 1
        case Triple | TSpinSingle     => 2
        case Tetris | TSpinDouble     => 4
        case TSpinTriple              => 6
        case _                        => 0
      }
      val bonusAttackTimes = (s2.backToBack, s2.allClear) match {
        case (true, true)  => 5
        case (true, false) => 1
        case (false, true) => 4
        case _             => 0
      }
      val comboAttackTimes = s2.combo match {
        case 0            => 0
        case 1 | 2        => 1
        case 3 | 4        => 2
        case 5 | 6        => 3
        case 7 | 8 | 9    => 4
        case x if x >= 10 => 5
        case _ => 0
      }
      (0 to (attackTimes + bonusAttackTimes + comboAttackTimes)).drop(1)
        .foreach(_ => opponent ! Attack)
      stateActor ! SetState(s2.copy(hasNotYetAttacked = false))
    }
  }
}
