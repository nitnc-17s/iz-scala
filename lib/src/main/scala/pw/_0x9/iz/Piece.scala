package pw._0x9.iz

sealed trait PieceKind { def toInt: Int }
case object GarbageKind extends PieceKind { def toInt = -1 }
case object IKind extends PieceKind { def toInt = 0 }
case object JKind extends PieceKind { def toInt = 1 }
case object LKind extends PieceKind { def toInt = 2 }
case object OKind extends PieceKind { def toInt = 3 }
case object SKind extends PieceKind { def toInt = 4 }
case object TKind extends PieceKind { def toInt = 5 }
case object ZKind extends PieceKind { def toInt = 6 }

case object PieceKind {
  def apply(x: Int): PieceKind = x match {
    case -1 => GarbageKind
    case 0  => IKind
    case 1  => JKind
    case 2  => LKind
    case 3  => OKind
    case 4  => SKind
    case 5  => TKind
    case 6  => ZKind
  }
}

sealed trait RotateState {
  def toInt: Int
  def rotateCW: RotateState = RotateState(this.toInt + 1 % 4)
  def rotateCCW: RotateState = RotateState(this.toInt + 3 % 4)
}
case object Rotate0 extends RotateState { def toInt = 0 }
case object Rotate90 extends RotateState { def toInt = 1 }
case object Rotate180 extends RotateState { def toInt = 2 }
case object Rotate270 extends RotateState { def toInt = 3 }

case object RotateState {
  def apply(x: Int): RotateState = x match {
    case 0 => Rotate0
    case 1 => Rotate90
    case 2 => Rotate180
    case 3 => Rotate270
    case _ => Rotate0
  }
}

case class Piece(pos: (Double, Double), kind: PieceKind, rotateState: RotateState, locals: Seq[(Double, Double)]) {
  import pw._0x9.iz.Piece._

  def current: Seq[Block] =
    locals map { case (x, y) =>
      Block((math.floor(x + pos._1).toInt, math.floor(y + pos._2).toInt), kind)
    }
  def moveBy(delta: (Double, Double)): Piece =
    copy(pos = (pos._1 + delta._1, pos._2 + delta._2))
  def rotateBy(direction: RotateDirection, testNumber: Int = 0): Piece = {
    val (newRotateState, theta) = direction match {
      case RotateCW  => (rotateState.rotateCW, -math.Pi / 2)
      case RotateCCW => (rotateState.rotateCCW, math.Pi / 2)
    }
    val (offsetX, offsetY) = calcOffset(rotateState, newRotateState, testNumber)
    val c = math.cos(theta)
    val s = math.sin(theta)
    def roundToHalf(v: (Double, Double)): (Double, Double) =
      (math.round(v._1 * 2.0) * 0.5, math.round(v._2 * 2.0) * 0.5)
    val p1 = copy(
      locals = locals map {
        case(x, y) => (x * c - y * s, x * s + y * c)
      } map roundToHalf map {
        case(x, y) => (x + offsetX, y + offsetY)
      },
      rotateState = newRotateState
    )
    p1
  }

  private[this] def calcOffset(oldRotateState: RotateState, newRotateState: RotateState, testNumber: Int = 0): (Int, Int) = {
    val JLSTZOffset = Seq(
      Seq(( 0, 0), (-1, 0), (-1,+1), ( 0,-2), (-1,-2)), // 0   to 90
      Seq(( 0, 0), (+1, 0), (+1,-1), ( 0,+2), (+1,+2)), // 90  to 0
      Seq(( 0, 0), (+1, 0), (+1,-1), ( 0,+2), (+1,+2)), // 90  to 180
      Seq(( 0, 0), (-1, 0), (-1,+1), ( 0,-2), (-1,-2)), // 180 to 90
      Seq(( 0, 0), (+1, 0), (+1,+1), ( 0,-2), (+1,-2)), // 180 to 270
      Seq(( 0, 0), (-1, 0), (-1,-1), ( 0,+2), (-1,+2)), // 270 to 180
      Seq(( 0, 0), (-1, 0), (-1,-1), ( 0,+2), (-1,+2)), // 270 to 0
      Seq(( 0, 0), (+1, 0), (+1,+1), ( 0,-2), (+1,-2))  // 0   to 270
    )
    val IOffset = Seq(
      Seq(( 0, 0), (-2, 0), (+1, 0), (-2,-1), (+1,+2)), // 0   to 90
      Seq(( 0, 0), (+2, 0), (-1, 0), (+2,+1), (-1,-2)), // 90  to 0
      Seq(( 0, 0), (-1, 0), (+2, 0), (-1,+2), (+2,-1)), // 90  to 180
      Seq(( 0, 0), (+1, 0), (-2, 0), (+1,-2), (-2,+1)), // 180 to 90
      Seq(( 0, 0), (+2, 0), (-1, 0), (+2,+1), (-1,-2)), // 180 to 270
      Seq(( 0, 0), (-2, 0), (+1, 0), (-2,-1), (+1,+2)), // 270 to 180
      Seq(( 0, 0), (+1, 0), (-2, 0), (+1,-2), (-2,+1)), // 270 to 0
      Seq(( 0, 0), (-1, 0), (+2, 0), (-1,+2), (+2,-1))  // 0   to 270
    )
    val rotate = (oldRotateState, newRotateState) match {
      case (Rotate0,   Rotate90)  => 0
      case (Rotate90,  Rotate0)   => 1
      case (Rotate90,  Rotate180) => 2
      case (Rotate180, Rotate90)  => 3
      case (Rotate180, Rotate270) => 4
      case (Rotate270, Rotate180) => 5
      case (Rotate270, Rotate0)   => 6
      case (Rotate0, Rotate270)   => 7
      case _ => -1
    }

    kind match {
      case JKind | LKind | SKind | TKind | ZKind => JLSTZOffset(rotate)(testNumber)
      case IKind                                 => IOffset(rotate)(testNumber)
      case _                                     => (0, 0)
    }
  }
}
case object Piece {
  sealed trait RotateDirection
  case object RotateCW extends RotateDirection
  case object RotateCCW extends RotateDirection

  def apply(pos: (Double, Double), kind: PieceKind, rotateState: RotateState): Piece =
    Piece(pos, kind, rotateState, kind match {
      case IKind => Seq((-1.5, 0.5), (-0.5, 0.5), ( 0.5,  0.5), (1.5,  0.5))
      case JKind => Seq((-1.0, 1.0), (-1.0, 0.0), ( 0.0,  0.0), (1.0,  0.0))
      case LKind => Seq((-1.0, 0.0), ( 0.0, 0.0), ( 1.0,  0.0), (1.0,  1.0))
      case OKind => Seq((-0.5, 0.5), ( 0.5, 0.5), (-0.5, -0.5), (0.5, -0.5))
      case SKind => Seq((-1.0, 0.0), ( 0.0, 0.0), ( 0.0,  1.0), (1.0,  1.0))
      case TKind => Seq((-1.0, 0.0), ( 0.0, 0.0), ( 1.0,  0.0), (0.0,  1.0))
      case ZKind => Seq((-1.0, 1.0), ( 0.0, 1.0), ( 0.0,  0.0), (1.0,  0.0))
      case _ => Seq()
    })
  def apply(pos: (Double, Double), kind: PieceKind): Piece = apply(pos, kind, Rotate0)
}
