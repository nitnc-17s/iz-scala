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

  private[this] val gridSize = (10, 40)
  private[this] val initialState1 = Stage.newState(
    Nil,
    // 20 play field rows and 20 buffer zone rows
    gridSize, Stage.randomStream(new scala.util.Random),
    isPlayer = true
  )
  private[this] val initialState2 = Stage.newState(
    Nil, gridSize, Stage.randomStream(new scala.util.Random)
  )
  private[this] val system = ActorSystem("IZSystem")
  private[this] val stateActor1 = system.actorOf(Props(new StateActor(
    initialState1)), name = "stateActor1")
  private[this] val lockTimerActor1 = system.actorOf(Props(new LockTimerActor(
    1 second)), name = "lockTimerActor1")
  private[this] val stageActor1 = system.actorOf(Props(new StageActor(
    stateActor1, Some(lockTimerActor1))), name = "stageActor1")
  private[this] val stateActor2 = system.actorOf(Props(new StateActor(
    initialState2)), name = "stateActor2")
  private[this] val stageActor2 = system.actorOf(Props(new StageActor(
    stateActor2)), name = "stageActor2")
  private[this] val agentActor = system.actorOf(Props(new AgentActor(
    stageActor2)), name = "agentActor")
  private[this] val masterActor = system.actorOf(Props(new GameMasterActor(
    stateActor1, stateActor2, agentActor, config: Config)), name = "masterActor")
  private[this] val tickTimer1 = system.scheduler.scheduleAtFixedRate(
    0 millisecond, 1000 millisecond, stageActor1, Tick)
  private[this] val tickTimer2 = system.scheduler.scheduleAtFixedRate(
    0 millisecond, 1000 millisecond, stageActor2, Tick)

  masterActor ! StartGM

  def left():      Unit = { stageActor1 ! MoveLeft }
  def right():     Unit = { stageActor1 ! MoveRight }
  def rotateCW():  Unit = { stageActor1 ! RotateCW }
  def rotateCCW(): Unit = { stageActor1 ! RotateCCW }
  def hold():      Unit = { stageActor1 ! Hold }
  def softDrop():  Unit = { stageActor1 ! SoftDrop }
  def hardDrop():  Unit = { stageActor1 ! HardDrop }
  def views: (GameView, GameView) =
    (Await.result((stateActor1 ? GetView).mapTo[GameView], timeout.duration),
      Await.result((stateActor2 ? GetView).mapTo[GameView], timeout.duration))
}
