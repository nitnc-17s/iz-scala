package pw._0x9.iz.actors

import akka.actor._

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._

sealed trait LockTimerMessage
case object Start extends LockTimerMessage
case object Stop extends LockTimerMessage
case object Reset extends LockTimerMessage

class LockTimerActor(t: FiniteDuration) extends Actor {
  private[this] val system: ActorSystem = context.system
  private[this] implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  private[this] val duration: FiniteDuration = t
  private[this] val resetLimits = 15

  def receive: Receive = onMessage(0, None)

  private[this] def startScheduler: Cancellable =
    system.scheduler.scheduleOnce(duration, sender, Lock)

  private def onMessage(resetTimes: Int, scheduler: Option[Cancellable]): Receive = {
    case Start =>
      println(Start, resetTimes, scheduler)
      context.become(onMessage(0, Some(startScheduler)))
    case Stop =>
      println(Stop, resetTimes, scheduler)
      if (scheduler.nonEmpty) {
        scheduler.get.cancel()
        context.become(onMessage(0, None))
      }
    case Reset =>
      println(Reset, resetTimes, scheduler)
      if (scheduler.nonEmpty && resetTimes <= resetLimits) {
        scheduler.get.cancel()
        context.become(onMessage(resetTimes + 1, Some(startScheduler)))
      }
  }
}
