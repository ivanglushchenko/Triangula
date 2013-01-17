package triangula

import swing._

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel}
import scala.swing.event._
import java.awt.{Color, Graphics2D, Point, geom}

/**
 * Contains logic for solving the puzzle. 
 */
trait Solver extends GameDef {
	lazy val startingBoard: Board = Board.Board()
	
	/**
	 * Gets boards generated from the given list of boards.
	 */
	def nextBoards(boards: List[Board]): List[Board] = {
		val list = (for {
			board <- boards
			nextBoard <- removeSymmetricalBoards(board.nextBoards)
		} yield nextBoard) toList

		removeSymmetricalBoards(list)
	}
	
	/**
	 * Prunes symmetrical boards.
	 */
	def removeSymmetricalBoards(list: List[Board]): List[Board] = {
		def isSmaller(l1: List[Int], l2: List[Int]): Boolean = {
			if (l1.isEmpty) false
			else {
				if (l1.head == l2.head) isSmaller(l1.tail, l2.tail)
				else l1.head < l2.head 
			}
		}
		
		def getLowerSymmHash(h1: List[Int], h2: List[Int], m1: scala.collection.immutable.Map[Int, Int], m2: scala.collection.immutable.Map[Int, Int]): (List[Int], List[Int]) = {
			val h1s = h1 map(t => m1(t)) sortBy(t => t)
			val h2s = h2 map(t => m2(t)) sortBy(t => t)
			val r1 = if (isSmaller(h1, h1s)) h1 else h1s
			val r2 = if (isSmaller(h2, h2s)) h2 else h2s
			(r1, r2)
		}
		
		def compactify(l: List[Board], symMap1: scala.collection.immutable.Map[Int, Int], symMap2: scala.collection.immutable.Map[Int, Int]): List[Board] = {
			val sets = l groupBy(t => getLowerSymmHash(t.hashEdges, t.hashTriangles, symMap1, symMap2)) map(t => t._2.sort((b1, b2) => isSmaller(b1.hashEdges, b2.hashEdges))) toList

			for { symBoards <- sets; if !symBoards.tail.isEmpty } {
				for { boardToBeRemoved <- symBoards.tail} {
					symBoards.head.parents = boardToBeRemoved.parents ::: symBoards.head.parents
					for { affectedParent <- boardToBeRemoved.parents} {
						affectedParent.children = symBoards.head :: (affectedParent.children - boardToBeRemoved)
					}
				}
			}
			
			sets map(t => t.head)
		}

		def applyNextSymmetry(boards: List[Board], symm: List[(scala.collection.immutable.Map[Int, Int], scala.collection.immutable.Map[Int, Int])]): List[Board] =
			if (symm.isEmpty) boards
			else applyNextSymmetry(compactify(boards, symm.head._1, symm.head._2), symm.tail)
			
		applyNextSymmetry(list, symmetries)
	}
	
	/**
	 * Generates a stream of boards.
	 */
	def next(boards: List[Board]): Stream[Board] = {
		if (boards.isEmpty) Stream.Empty
		else {
			val l = boards.first.edges.length + 1
			println("..generating " + l.toString + "-edge boards")

			val list = nextBoards(boards)

			var c = list.length
			println("..done, there are " + c.toString + " of them")
			if (list.isEmpty) Stream.Empty
			else Stream.concat(list toStream, next(list))
		}
	}
	
	/**
	 * A stream of all possible boards.
	 */
	lazy val allBoardsStream: Stream[Board] = next(startingBoard.nextBoards)
	
	/**
	 * A stream of all completed boards.
	 */
	lazy val allCompletedBoardsStream = allBoardsStream filter (_.isCompleted)
	
	lazy val allCompletedBoards = {
		val boards = allCompletedBoardsStream.take(10000).toList
		println("..calc scores")
		startingBoard.scores
		println("..done")
		boards
	}
	
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
		preferredSize = (900,900)
		focusable = true
		
		val horOffset = 0
		val verOffset = 0
		val margin = 1;
		
		def dim = math.sqrt(boardsToDraw.length).toInt + 1
		def iRows = (for { i <- (0 until dim)} yield i) toList
		def iColumns = (for { i <- (0 until dim)} yield i) toList
		
		def boardWidth = preferredSize.width / dim
		def boardHeight = preferredSize.height / dim
		def pointHorOffset = (boardWidth - 2 * margin) / (width + 1)
		def pointVerOffset = (boardHeight - 2 * margin) / (height + 1)
		
		def drawCell(g: Graphics2D, iRow: Int, iColumn: Int) = {
			val boardIndex = iRow * dim + iColumn
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

				val p = (for (x <- (1 to width); y <- (1 to height)) yield (x, y)) toList
					
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
			val boardIndex = row * dim + col
			if (boardsToDraw.length > boardIndex)
				boardsToDraw(boardIndex)
			else
				null
		}
	
		listenTo(this.mouse.clicks)
		
		reactions += {
			case MouseClicked(e1,e2,e3,e4,e5) => {
				val board = getBoard(e2);
				//println("b: " + board)
				//println("b1: " + board.hashEdges)
				//println("b2: " + board.hashTriangles)
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