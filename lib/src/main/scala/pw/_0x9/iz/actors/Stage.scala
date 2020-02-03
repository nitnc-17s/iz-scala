package pw._0x9.iz.actors

import akka.actor._
import akka.pattern._
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
case object SoftDrop extends StageMessage
case object HardDrop extends StageMessage
case object Attack extends StageMessage
case object Lock extends StageMessage

class StageActor(stateActor: ActorRef, lockTimerActor: Option[ActorRef] = None) extends Actor {
  import Stage._

  def receive: Receive = {
    case MoveLeft  => updateState {moveLeft}
    case MoveRight => updateState {moveRight}
    case RotateCW  => updateState {rotateCW}
    case RotateCCW => updateState {rotateCCW}
    case Hold      => updateState {hold}
    case Tick      => updateState {tick}
    case SoftDrop  => updateState {softDrop}
    case HardDrop  => updateState {hardDrop}
    case Attack    => updateState {notifyAttack}
    case Lock      => updateState {lock}
  }
  private[this] def opponent: ActorSelection =
    if (self.path.name == "stageActor1") context.actorSelection("/user/stageActor2")
    else context.actorSelection("/user/stageActor1")
  private[this] def updateState(trans: GameState => GameState): Unit = {
    val future = (stateActor ? GetState)(1 second).mapTo[GameState]
    val s1 = Await.result(future, 1 second)
    val s2 = trans(s1)
    val s3 = if (s2.isPlayer && lockTimerActor.nonEmpty) {
      s2.lockTimerOperation match {
        case StartTimer => lockTimerActor.get ! Start
        case StopTimer => lockTimerActor.get ! Stop
        case ResetTimer => lockTimerActor.get ! Reset
        case _ => // do nothing
      }
      s2.copy(lockTimerOperation = NoOperation)
    } else {
      s2.lockTimerOperation match {
        case StartTimer => lock(s2).copy(lockTimerOperation = NoOperation)
        case _ => s2
      }
    }
    stateActor ! SetState(s3)

    if (s3.hasNotYetAttacked) {
      val attackTimes = s3.lastClear match {
        case Single | MiniTSpinSingle => 0
        case Double | MiniTSpinDouble => 1
        case Triple | TSpinSingle     => 2
        case Tetris | TSpinDouble     => 4
        case TSpinTriple              => 6
        case _                        => 0
      }
      val bonusAttackTimes = (s3.backToBack, s3.allClear) match {
        case (true, true)  => 5
        case (true, false) => 1
        case (false, true) => 4
        case _             => 0
      }
      val comboAttackTimes = s3.combo match {
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
      stateActor ! SetState(s3.copy(hasNotYetAttacked = false))
    }
  }
}
