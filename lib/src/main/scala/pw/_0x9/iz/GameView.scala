package pw._0x9.iz

case class GameView(
  blocks: Seq[Block], gridSize: (Int, Int),
  current: Seq[Block], miniGridSize: (Int, Int), next: Seq[Seq[Block]],
  status: GameStatus, lineCount: Int, ghost: Seq[Block],
  hold: Seq[Block], alreadyHold: Boolean, lastClear: ClearType,
  backToBack: Boolean, allClear: Boolean, combo: Int,
  pendingAttackOnYourself: Int
)
