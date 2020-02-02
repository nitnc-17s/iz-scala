package pw._0x9.iz.actors

case class Config(
  minActionTime: Long,
  maxThinkTime: Long,
  onDrop: Option[StageMessage])
