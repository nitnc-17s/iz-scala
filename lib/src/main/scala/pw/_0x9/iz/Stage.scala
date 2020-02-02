package pw._0x9.iz

import scala.util.Random

object Stage {
  import scala.annotation.tailrec
  import pw._0x9.iz.actors._

  def bagOf7(random: Random): Iterator[PieceKind] = {
    val listOf7: Seq[Int] = 0 until 7
    val pieceKinds = random.shuffle(listOf7).map(PieceKind(_))
    pieceKinds.iterator
  }
  def randomStream(random: Random): LazyList[PieceKind] =
    LazyList.from(bagOf7(random)) #::: randomStream(random)
  def newState(blocks: Seq[Block], gridSize: (Int, Int),
    kinds: Seq[PieceKind]): GameState = {
    val dummy = Piece((0, 0), TKind)
    val withNext = spawn(GameState(Nil, gridSize, dummy, Seq() padTo (6, dummy), kinds)).
      copy(blocks = blocks)
    spawn(withNext)
  }
  val moveLeft: GameState => GameState = ghost compose transit { _.moveBy(-1.0, 0.0) }
  val moveRight: GameState => GameState = ghost compose transit { _.moveBy(1.0, 0.0) }
  val rotateCW: GameState => GameState = ghost compose rotate(Piece.RotateCW)
  val rotateCCW: GameState => GameState = ghost compose rotate(Piece.RotateCCW)
  val hold: GameState => GameState = ghost compose holdSwap
  val tick: GameState => GameState = ghost compose transit(_.moveBy(0.0, -1.0),
    Function.chain(clearFullRow :: attack :: spawn :: resetAlreadyHold :: Nil))
  val drop: GameState => GameState = (s0: GameState) =>
    Function.chain((Nil padTo (s0.gridSize._2, transit {_.moveBy(0.0, -1.0)})) ++
      List(tick))(s0)
  val notifyAttack: GameState => GameState = (s0: GameState) =>
    s0.copy(pendingAttacks = s0.pendingAttacks + 1)
  private[this] lazy val clearFullRow: GameState => GameState =
    (s0: GameState) => {
      def isFullRow(i: Int, s: GameState): Boolean =
        (s.blocks count {
          _.pos._2 == i
        }) == s.gridSize._1
      @tailrec def tryRow(i: Int, s: GameState): GameState =
        if (i < 0) s
        else if (isFullRow(i, s))
          tryRow(i - 1, s.copy(blocks = (s.blocks filter {_.pos._2 < i}) ++
            (s.blocks filter {_.pos._2 > i} map { b =>
              b.copy(pos = (b.pos._1, b.pos._2 - 1)) }),
            lastDeleted = s.lastDeleted + 1))
        else tryRow(i - 1, s)
      val s1 = tryRow(s0.gridSize._2 - 1, s0)
      if (s1.lastDeleted == 0) s1
      else s1.copy(lineCounts = s1.lineCounts updated
        (s1.lastDeleted, s1.lineCounts(s1.lastDeleted) + 1))
    }
  val attackRandom = new util.Random(0L)
  private[this] lazy val attack: GameState => GameState =
    (s0: GameState) => {
      def attackRow(s: GameState): Seq[Block] = {
        val nonFillCol = attackRandom.nextInt(s.gridSize._1)
        (0 until s.gridSize._1) flatMap { x =>
          if (x != nonFillCol) Some(Block((x, 0), GarbageKind))
          else None
        }
      }
      @tailrec def tryAttack(s: GameState): GameState =
        if (s.pendingAttacks < 1) s
        else tryAttack(s.copy(
          blocks = (s.blocks map { b => b.copy(pos = (b.pos._1, b.pos._2 + 1)) } filter {
            _.pos._2 < s.gridSize._2 }) ++ attackRow(s),
          pendingAttacks = s.pendingAttacks - 1
        ))
      tryAttack(s0)
    }
  private[this] lazy val spawn: GameState => GameState =
    (s: GameState) => {
      val nextPiecesLength = 6
      def dropOffPos = (s.gridSize._1 / 2.0, s.gridSize._2 - 2.0)
      val s1 = s.copy(blocks = s.blocks,
        currentPiece = s.nextPieces.head.copy(pos = dropOffPos),
        nextPieces = s.kinds.take(nextPiecesLength).map(Piece((2, 1), _)),
        kinds = s.kinds.drop(1))
      validate(s1) map (x =>
        x.load(x.currentPiece)) getOrElse {
        s1.load(s1.currentPiece).copy(status = GameOver)
      }
    }
  private[this] lazy val ghost: GameState => GameState =
    (s: GameState) => {
      if (s.isPlayer) {
        val s1 = Function.chain(Nil padTo(s.gridSize._2, transit {
          _.moveBy(0.0, -1.0)
        }))(s)
        s.copy(ghost = s1.currentPiece.current)
      }
      else s
    }
  private[this] lazy val holdSwap: GameState => GameState =
    (s: GameState) => {
      if (!s.alreadyHold) {
        if (s.hold.isEmpty) {
          spawn(s.unload(s.currentPiece)).copy(
            hold = Some(Piece((2, 1), s.currentPiece.kind)),
            alreadyHold = true
          )
        } else {
          def dropOffPos = (s.gridSize._1 / 2.0, s.gridSize._2 - 2.0)
          s.unload(s.currentPiece).copy(
            currentPiece = s.hold.get.copy(pos = dropOffPos),
            hold = Some(Piece((2, 1), s.currentPiece.kind)),
            alreadyHold = true
          )
        }
      }
      else s
    }
  private[this] lazy val resetAlreadyHold: GameState => GameState =
    (s: GameState) => {
      s.copy(alreadyHold = false)
    }
  private[this] def rotate(rotateDirection: Piece.RotateDirection): GameState => GameState =
    (s: GameState) => {
      s.currentPiece.kind match {
        case GarbageKind => s
        case OKind =>
          transit({ _.rotateBy(rotateDirection) })(s)
        case _ =>
          transit({ _.rotateBy(rotateDirection) },
            transit({ _.rotateBy(rotateDirection, 1)},
              transit({ _.rotateBy(rotateDirection, 2)},
                transit({ _.rotateBy(rotateDirection, 3)},
                  transit({ _.rotateBy(rotateDirection, 4)})
                )
              )
            )
          )(s)
      }
    }
  private[this] def transit(trans: Piece => Piece,
    onFail: GameState => GameState = identity): GameState => GameState =
    (s: GameState) => s.status match {
      case ActiveStatus =>
        validate(s.unload(s.currentPiece).copy(
          currentPiece = trans(s.currentPiece),
          lastDeleted = 0)) map (x =>
          x.load(x.currentPiece)) getOrElse {onFail(s)}
      case _ => s
    }
  private[this] def validate(s: GameState): Option[GameState] = {
    val size = s.gridSize
    def inBounds(pos: (Int, Int)): Boolean =
      (pos._1 >= 0) && (pos._1 < size._1) && (pos._2 >= 0) && (pos._2 < size._2)
    val currentPoss = s.currentPiece.current map {_.pos}
    if ((currentPoss forall inBounds) &&
      (s.blocks map {_.pos} intersect currentPoss).isEmpty) Some(s)
    else None
  }
  def toTrans(message: StageMessage): GameState => GameState =
    message match {
      case MoveLeft  => moveLeft
      case MoveRight => moveRight
      case RotateCW  => rotateCW
      case RotateCCW => rotateCCW
      case Hold      => hold
      case Tick      => tick
      case Drop      => drop
      case Attack    => notifyAttack
    }
}
