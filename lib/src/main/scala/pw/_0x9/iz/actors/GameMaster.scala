package pw._0x9.iz.actors

import akka.actor._
import akka.pattern.ask
import pw._0x9.iz._

import scala.concurrent.Await
import scala.concurrent.duration._

sealed trait GameMasterMessage
case object Start

class GameMasterActor(stateActor1: ActorRef, stateActor2: ActorRef,
  agentActor: ActorRef, config: Config) extends Actor {
  def receive: Receive = {
    case Start => loop()
  }
  private[this] def loop(): Unit = {
    var s = getStatesAndJudge._2
    while (s.status == Active) {
      val t0 = System.currentTimeMillis
      val future = (agentActor ? BestMoves(getState2, config))(60 second)
      Await.result(future, 60 second)
      val t1 = System.currentTimeMillis
      if (t1 - t0 < config.minActionTime) Thread.sleep(config.minActionTime - (t1 - t0))
      s = getStatesAndJudge._2
    }
  }
  private[this] def getStatesAndJudge: (GameState, GameState) = {
    var s1 = getState1
    var s2 = getState2
    if (s1.status == GameOver && s2.status != Victory) {
      stateActor2 ! SetState(s2.copy(status = Victory))
      s2 = getState2
    }
    if (s1.status != Victory && s2.status == GameOver) {
      stateActor1 ! SetState(s1.copy(status = Victory))
      s1 = getState1
    }
    (s1, s2)
  }
  private[this] def getState1: GameState = {
    val future = (stateActor1 ? GetState)(1 second).mapTo[GameState]
    Await.result(future, 1 second)
  }
  private[this] def getState2: GameState = {
    val future = (stateActor2 ? GetState)(1 second).mapTo[GameState]
    Await.result(future, 1 second)
  }
}
