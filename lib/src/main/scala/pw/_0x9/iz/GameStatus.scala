package pw._0x9.iz

sealed trait GameStatus
case object Active extends GameStatus
case object GameOver extends GameStatus
case object Victory extends GameStatus
