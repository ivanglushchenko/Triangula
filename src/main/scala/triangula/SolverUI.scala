package triangula

import swing._

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel}
import scala.swing.event._
import java.awt.{Color, Graphics2D, Point, geom}

trait SolverUI extends Solver {
	/**
	 * Simple UI for showing boards and scores.
	 */
	lazy val ui = new Panel {
		var allBoardsToDraw = {
		  allCompletedBoards
		  List(startingBoard.children.take(100))
		}
		def boardsToDraw = allBoardsToDraw.head
		
		background = Color.white
		preferredSize = (800, 800)
		focusable = true
		
		val horOffset = 0
		val verOffset = 0
		val margin = 1;
		
		def uiDim = math.sqrt(boardsToDraw.length).toInt + 1
		def iRows = (for { i <- (0 until uiDim)} yield i) toList
		def iColumns = (for { i <- (0 until uiDim)} yield i) toList
		
		def boardWidth = preferredSize.width / uiDim
		def boardHeight = preferredSize.height / uiDim
		def pointHorOffset = (boardWidth - 2 * margin) / (dim.width + 1)
		def pointVerOffset = (boardHeight - 2 * margin) / (dim.height + 1)
		
		def drawCell(g: Graphics2D, iRow: Int, iColumn: Int) = {
			val boardIndex = iRow * uiDim + iColumn
			if (boardsToDraw.length > boardIndex) {
				val board = boardsToDraw(boardIndex)
				
				g.setColor(Color.BLACK)
				
				// draw border around the board
				var path = new geom.GeneralPath
				path.moveTo(iColumn * boardWidth + margin, iRow * boardHeight + margin)
				path.lineTo((iColumn + 1) * boardWidth - margin, iRow * boardHeight + margin)
				path.lineTo((iColumn + 1) * boardWidth - margin, (iRow + 1) * boardHeight - margin)
				path.lineTo(iColumn * boardWidth + margin, (iRow + 1) * boardHeight - margin)
				path.lineTo(iColumn * boardWidth + margin, iRow * boardHeight + margin)
				g.draw(path)
			
				val total = board.scores.map(t => t._2).sum toFloat
				val p1 = math.round(100.0 * board.scores(Player1) / total)
				val p2 = math.round(100.0 * board.scores(Player2) / total)
				val pU = math.round(100.0 * board.scores(PlayerUnknown) / total)
				val font = new Font("Consolas", 1 , 10)
				g.setFont(font)
				val fontMetrics = g.getFontMetrics(font)
				g.drawString("next: " + board.nextPlayer, iColumn * boardWidth + 4, ((iRow) * boardHeight).toInt + 12)
				val t1 = "score: " + board.player1Area.toInt.toString + ":" + board.player2Area.toInt.toString
				g.drawString(t1, ((iColumn + 1) * boardWidth - fontMetrics.stringWidth(t1) - 4).toInt, ((iRow) * boardHeight).toInt + 12)
				g.drawString("W: %d%% L:%d%% D:%d%%".format(p1, p2, pU), iColumn * boardWidth + 4, ((iRow + 1) * boardHeight).toInt - 4)

				val p = (for (x <- (1 to dim.width); y <- (1 to dim.height)) yield (x, y)) toList
					
				def pointToX(p: Pos): Int = iColumn * boardWidth + margin + p.y * pointHorOffset
				def pointToY(p: Pos): Int = iRow * boardHeight + margin + p.x * pointVerOffset
				
				board.edges.foreach(e => {
					val color = if (e == board.edges.head) Color.BLUE else Color.BLACK
					g.setColor(color)
					g.drawLine(pointToX(e.from), pointToY(e.from), pointToX(e.to), pointToY(e.to))
				})
				
				board.triangles.foreach(t => {
					val color = if (t.player == Player1) Color.GREEN else Color.RED
					g.setColor(color)
					g.fillPolygon(Array(pointToX(t.p1), pointToX(t.p2), pointToX(t.p3)), Array(pointToY(t.p1), pointToY(t.p2), pointToY(t.p3)), 3)
				})
				
				board.points.foreach (t => {
					g.setColor(Color.GRAY)
					g.fillOval(iColumn * boardWidth + margin + pointHorOffset * t.x - 2, iRow * boardHeight + margin + pointVerOffset * t.y - 2, 4, 4)
				})
			}
		}
				
		override def paintComponent(g: Graphics2D) = {
			super.paintComponent(g)
			iRows.foreach(i => iColumns.foreach(j => drawCell(g, i, j)))
		}
		
		def getBoard(p: Point) : Board = {
			val col = p.x / boardWidth
			val row = p.y / boardHeight
			val boardIndex = row * uiDim + col
			if (boardsToDraw.length > boardIndex)
				boardsToDraw(boardIndex)
			else
				null
		}
	
		listenTo(this.mouse.clicks)
		
		reactions += {
			case MouseClicked(e1,e2,e3,e4,e5) => {
				val board = getBoard(e2);
				if (e3 == 0)
				{
					if (board != null && !board.isCompleted){
						allBoardsToDraw = board.children :: allBoardsToDraw
						repaint()
					}
				}
				if (e3 == 256)
				{
					if (allBoardsToDraw.length > 1){
						allBoardsToDraw = allBoardsToDraw.tail
						repaint()
					}
				}
			}
			
		}
	}
}