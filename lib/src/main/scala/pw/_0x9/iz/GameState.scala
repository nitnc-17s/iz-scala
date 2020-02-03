package pw._0x9.iz

sealed trait ClearType
case object NoClear extends ClearType
case object Single extends ClearType
case object Double extends ClearType
case object Triple extends ClearType
case object Tetris extends ClearType
case object TSpinSingle extends ClearType
case object MiniTSpinSingle extends ClearType
case object TSpinDouble extends ClearType
case object MiniTSpinDouble extends ClearType
case object TSpinTriple extends ClearType

sealed trait LockTimerOperation
case object NoOperation extends LockTimerOperation
case object StartTimer extends LockTimerOperation
case object StopTimer extends LockTimerOperation
case object ResetTimer extends LockTimerOperation

case class GameState(blocks: Seq[Block], gridSize: (Int, Int),
  currentPiece: Piece, nextPieces: Seq[Piece], kinds: Seq[PieceKind],
  status: GameStatus = Active,
  lineCounts: Seq[Int] = Seq(0, 0, 0, 0, 0),
  ghost: Seq[Block] = Nil,
  hold: Option[Piece] = None,
  alreadyHold: Boolean = false,
  pendingAttacksOnYourself: Int = 0,
  lastDeleted: Int = 0,
  lastOperationIsRotate: Boolean = false,
  lastRotateUseSRS: Boolean = false,
  lastClear: ClearType = NoClear,
  backToBack: Boolean = false,
  allClear: Boolean = false,
  combo: Int = 0,
  hasNotYetAttacked: Boolean = false,
  isPlayer: Boolean = false,
  lockTimerOperation: LockTimerOperation = NoOperation,
  lastOperationIsMoveOrRotate: Boolean = false
) {
  def lineCount: Int =
    lineCounts.zipWithIndex map { case (n, i) => n * i } sum
  def view: GameView = {
    val holdBlocks = if (hold.nonEmpty) hold.get.current else Nil
    GameView(
      blocks, gridSize,
      currentPiece.current, (4, 4), nextPieces.map(_.current),
      status, lineCount, ghost, holdBlocks, alreadyHold,
      lastClear, backToBack, allClear, combo,
      pendingAttacksOnYourself
    )
  }
  def unload(p: Piece): GameState = {
    val currentPoss = p.current map {_.pos}
    this.copy(blocks = blocks filterNot { currentPoss contains _.pos })
  }
  def load(p: Piece): GameState =
    this.copy(blocks = blocks ++ p.current)
}
