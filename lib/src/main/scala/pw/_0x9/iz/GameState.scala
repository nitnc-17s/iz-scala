package pw._0x9.iz

case class GameState(blocks: Seq[Block], gridSize: (Int, Int),
  currentPiece: Piece, nextPieces: Seq[Piece], kinds: Seq[PieceKind],
  status: GameStatus = ActiveStatus,
  lineCounts: Seq[Int] = Seq(0, 0, 0, 0, 0),
  ghost: Seq[Block] = Nil,
  hold: Option[Piece] = None,
  alreadyHold: Boolean = false,
  lastDeleted: Int = 0, pendingAttacks: Int = 0, isPlayer: Boolean = false) {
  def lineCount: Int =
    lineCounts.zipWithIndex map { case (n, i) => n * i } sum
  def attackCount: Int =
    lineCounts.drop(1).zipWithIndex map { case (n, i) => n * i } sum
  def view: GameView = {
    val holdBlocks = if (hold.nonEmpty) hold.get.current else Nil
    GameView(blocks, gridSize,
      currentPiece.current, (4, 4), nextPieces.map(_.current),
      status, lineCount, ghost, holdBlocks, alreadyHold)
  }
  def unload(p: Piece): GameState = {
    val currentPoss = p.current map {_.pos}
    this.copy(blocks = blocks filterNot { currentPoss contains _.pos })
  }
  def load(p: Piece): GameState =
    this.copy(blocks = blocks ++ p.current)
}
