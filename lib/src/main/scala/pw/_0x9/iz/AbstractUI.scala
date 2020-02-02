package pw._0x9.iz

import pw._0x9.iz.actors._

class AbstractUI(config: Config) {
  import akka.actor._
  import akka.pattern.ask
  import akka.util.Timeout

  import scala.concurrent._
  import scala.concurrent.duration._
  import ExecutionContext.Implicits.global
  implicit val timeout: Timeout = Timeout(100 millisecond)

  private[this] val initialState = Stage.newState(Nil,
    (10, 23), Stage.randomStream(new scala.util.Random))
  private[this] val system = ActorSystem("IZSystem")
  private[this] val stateActor1 = system.actorOf(Props(new StateActor(
    initialState.copy(isPlayer = true))), name = "stateActor1")
  private[this] val stageActor1 = system.actorOf(Props(new StageActor(
    stateActor1)), name = "stageActor1")
  private[this] val stateActor2 = system.actorOf(Props(new StateActor(
    initialState)), name = "stateActor2")
  private[this] val stageActor2 = system.actorOf(Props(new StageActor(
    stateActor2)), name = "stageActor2")
  private[this] val agentActor = system.actorOf(Props(new AgentActor(
    stageActor2)), name = "agentActor")
  private[this] val masterActor = system.actorOf(Props(new GameMasterActor(
    stateActor1, stateActor2, agentActor, config: Config)), name = "masterActor")
  private[this] val tickTimer1 = system.scheduler.scheduleAtFixedRate(
    0 millisecond, 701 millisecond, stageActor1, Tick)
  private[this] val tickTimer2 = system.scheduler.scheduleAtFixedRate(
    0 millisecond, 701 millisecond, stageActor2, Tick)

  masterActor ! Start

  def left()      { stageActor1 ! MoveLeft }
  def right()     { stageActor1 ! MoveRight }
  def rotateCW()  { stageActor1 ! RotateCW }
  def rotateCCW() { stageActor1 ! RotateCCW }
  def hold()      { stageActor1 ! Hold }
  def softDrop()  { stageActor1 ! Tick }
  def hardDrop()  { stageActor1 ! Drop }
  def views: (GameView, GameView) =
    (Await.result((stateActor1 ? GetView).mapTo[GameView], timeout.duration),
      Await.result((stateActor2 ? GetView).mapTo[GameView], timeout.duration))
}
