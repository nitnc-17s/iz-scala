package pw._0x9.iz.fx

import pw._0x9.iz._
import pw._0x9.iz.actors._
import scalafx.Includes._
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.Image
import scalafx.scene.input.KeyCode._
import scalafx.scene.input.KeyEvent
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.text.{Font, Text, TextAlignment, TextFlow}

object Main extends JFXApp {
  val config: Config = Config(minActionTime = 151,
    maxThinkTime = 1500,
    onDrop = Some(Tick))
  val ui = new AbstractUI(config)

  def onKeyPress(keyEvent: KeyEvent): Unit = keyEvent.code match {
    case Left    => ui.left()
    case Right   => ui.right()
    case Up      => ui.rotateCW()
    case Down    => ui.softDrop()
    case Space   => ui.hardDrop()
    case Control => ui.rotateCCW()
    case Shift   => ui.hold()
    case _ =>
  }

  private val cl = getClass.getClassLoader
  val I_BLOCK       = new Image(cl.getResourceAsStream("img/blocks/I.png"))
  val J_BLOCK       = new Image(cl.getResourceAsStream("img/blocks/J.png"))
  val L_BLOCK       = new Image(cl.getResourceAsStream("img/blocks/L.png"))
  val O_BLOCK       = new Image(cl.getResourceAsStream("img/blocks/O.png"))
  val S_BLOCK       = new Image(cl.getResourceAsStream("img/blocks/S.png"))
  val T_BLOCK       = new Image(cl.getResourceAsStream("img/blocks/T.png"))
  val Z_BLOCK       = new Image(cl.getResourceAsStream("img/blocks/Z.png"))
  val GARBAGE_BLOCK = new Image(cl.getResourceAsStream("img/blocks/Garbage.png"))

  val BLOCK_IMG_SIZE: Double = I_BLOCK.width.value
  val BLOCK_SIZE: Double = BLOCK_IMG_SIZE
  val COLS: Int = 10
  val ROWS: Int = 20
  val w: Double = BLOCK_SIZE * COLS
  val h: Double = BLOCK_SIZE * ROWS
  val gameGrid: Canvas = new Canvas(w, h) {
    margin = Insets(0, BLOCK_SIZE, 0, BLOCK_SIZE)
  }
  def createMiniGrid: Canvas = {
    new Canvas(BLOCK_SIZE * 2, BLOCK_SIZE * 2) {
      margin = Insets(BLOCK_SIZE / 2, BLOCK_SIZE / 5, 0, BLOCK_SIZE / 5)
    }
  }
  val nextGrids: Seq[Canvas] = Seq(
    createMiniGrid,
    createMiniGrid,
    createMiniGrid,
    createMiniGrid,
    createMiniGrid,
    createMiniGrid
  )
  val holdGrid: Canvas = createMiniGrid
  val alreadyHoldText: Text = new Text { stroke = White }
  val gameStatusText: Text = new Text { stroke = White }
  val lineCountsText: Text = new Text { stroke = White }
  val clearTypeText: Text = new Text { stroke = White }
  val backToBackText: Text = new Text { stroke = White }
  val allClearText: Text = new Text { stroke = White }
  val comboText: Text = new Text { stroke = White }
  val cpuGameGrid: Canvas = new Canvas(w, h) {
    margin = Insets(0, BLOCK_SIZE, 0, BLOCK_SIZE)
  }

  stage = new PrimaryStage {
    showing.addListener((_, oldValue, newValue) => {
      if (oldValue && !newValue) {
        sys.exit()
      }
    })
    title.value = "iz"
    resizable = false
    scene = new Scene {
      fill = Black
      onKeyPressed = onKeyPress
      root = new BorderPane {
        padding = Insets(BLOCK_SIZE / 2)
        left = new BorderPane {
          center = gameGrid
          left = new VBox {
            children = Seq(
              new TextFlow {
                textAlignment = TextAlignment.Center
                children = new Text("HOLD") {
                  font = Font(24)
                  fill = White
                }
              },
              holdGrid,
              new TextFlow {
                textAlignment = TextAlignment.Center
                children = alreadyHoldText
              },
              new TextFlow {
                margin = Insets(BLOCK_SIZE, 0, 0, 0)
                children = clearTypeText
              },
              new TextFlow {
                children = backToBackText
              },
              new TextFlow {
                children = allClearText
              },
              new TextFlow {
                children = comboText
              }
            )
          }
          right = new BorderPane {
            top = new VBox {
              children = Seq(
                new TextFlow {
                  textAlignment = TextAlignment.Center
                  children = new Text("NEXT") {
                    font = Font(24)
                    fill = White
                  }
                }
              ) ++ nextGrids
            }
            bottom = new VBox {
              children = Seq(
                new TextFlow {
                  children = gameStatusText
                },
                new TextFlow {
                  children = lineCountsText
                }
              )
            }
          }
        }
        right = cpuGameGrid
      }
    }
  }

  val timer: AnimationTimer = AnimationTimer { time =>
    onPaint()
  }
  timer.start()

  private def onPaint(): Unit = {
    val (view1, view2) = ui.views

    drawGame(view1, gameGrid)
    nextGrids.zipWithIndex.foreach({
      case (nextGrid, i) => drawMini(nextGrid, view1.next(i))
    })
    drawMini(holdGrid, view1.hold)
    drawText()
    drawGame(view2, cpuGameGrid)

    def drawGame(view: GameView, gameGrid: Canvas): Unit = {
      val g = gameGrid.graphicsContext2D
      val gridSize = (10, 20)
      val blocks = view.blocks
      val current = view.current
      val ghost = view.ghost

      val blockSize = BLOCK_SIZE
      val w = blockSize * gridSize._1
      val h = blockSize * gridSize._2
      def clear(): Unit = {
        g.clearRect(0, 0, w, h)
      }
      def drawEmptyGrid(): Unit = {
        g.stroke = Color.web("#28282E")
        g.lineWidth = 2
        for (x <- 0 to view.gridSize._1) {
          g.strokeLine(x * blockSize, 0, x * blockSize, h)
        }
        for (y <- 0 to view.gridSize._2) {
          g.strokeLine(0, y * blockSize, w, y * blockSize)
        }
      }
      def drawBlock(b: Block, isGhost: Boolean = false): Unit = {
        val blockImage = b.kind match {
          case IKind => I_BLOCK
          case JKind => J_BLOCK
          case LKind => L_BLOCK
          case OKind => O_BLOCK
          case SKind => S_BLOCK
          case TKind => T_BLOCK
          case ZKind => Z_BLOCK
          case GarbageKind => GARBAGE_BLOCK
        }

        if (isGhost) {
          val prevGlobalAlpha = g.globalAlpha
          g.globalAlpha = 0.25
          g.drawImage(blockImage, b.pos._1 * blockSize, h - (b.pos._2 + 1) * blockSize,
            blockSize, blockSize)
          g.globalAlpha = prevGlobalAlpha
        } else {
          g.drawImage(blockImage, b.pos._1 * blockSize, h - (b.pos._2 + 1) * blockSize,
            blockSize, blockSize)
        }
      }
      def drawSeqBlock(blocks: Seq[Block], isGhost: Boolean = false): Unit = {
        blocks filter {_.pos._2 < gridSize._2 } foreach { b => drawBlock(b, isGhost) }
      }
      def drawBlocks(): Unit = drawSeqBlock(blocks)
      def drawCurrent(): Unit = drawSeqBlock(current)
      def drawGhost(): Unit = drawSeqBlock(ghost, isGhost = true)

      clear()
      drawEmptyGrid()
      drawBlocks()
      drawCurrent()
      drawGhost()
    }

    def drawMini(miniGrid: Canvas, blocks: Seq[Block]): Unit = {
      val g = miniGrid.graphicsContext2D
      val gridSize = (4, 4)

      val blockSize = BLOCK_SIZE / 2
      val w = blockSize * gridSize._1
      val h = blockSize * gridSize._2

      def clear(): Unit = {
        g.clearRect(0, 0, w, h)
      }

      def drawBlock(b: Block): Unit = {
        val blockImage = b.kind match {
          case IKind => I_BLOCK
          case JKind => J_BLOCK
          case LKind => L_BLOCK
          case OKind => O_BLOCK
          case SKind => S_BLOCK
          case TKind => T_BLOCK
          case ZKind => Z_BLOCK
          case GarbageKind => GARBAGE_BLOCK
        }

        g.drawImage(blockImage, b.pos._1 * blockSize, h - (b.pos._2 + 1) * blockSize,
          blockSize, blockSize)
      }

      def drawMini(): Unit = {
        blocks foreach { b => drawBlock(b) }
      }

      clear()
      drawMini()
    }

    def drawText(): Unit = {
      val view = view1
      gameStatusText.text = view.status match {
        case Active => "Status: Active"
        case GameOver => "Status: Game Over"
        case Victory => "Status: Victory"
      }
      lineCountsText.text = s"Lines: ${view.lineCount}"
      alreadyHoldText.text =
        if (view.alreadyHold) "Already Hold" else ""
      clearTypeText.text = view.lastClear match {
        case NoClear         => ""
        case Single          => "Single"
        case Double          => "Double"
        case Triple          => "Triple"
        case Tetris          => "TETRIS!"
        case TSpinSingle     => "T-Spin Single!"
        case MiniTSpinSingle => "Mini T-Spin Single"
        case TSpinDouble     => "T-Spin Double!!"
        case MiniTSpinDouble => "Mini T-Spin Double"
        case TSpinTriple     => "T-Spin Triple!!!"
      }
      backToBackText.text =
        if (view.backToBack) "Back-to-Back" else ""
      allClearText.text =
        if (view.allClear) "ALL CLEAR!!!" else ""
      comboText.text =
        if (view.combo >= 1) s"${view.combo} Ren" else ""
    }
  }
}
