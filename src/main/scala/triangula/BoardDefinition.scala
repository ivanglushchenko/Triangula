package triangula

import scala.collection.immutable._
import scala.collection.immutable.Map

/**
 * Contains game components/logic.
 */
trait BoardDefinition {
	val dim: BoardDimension
	
	lazy val allPoints = 
		(for {
			x <- (1 to dim.width)
			y <- (1 to dim.height)
		} yield Pos(x, y)) toList
		
	def generateEdges(points: List[Pos]): List[Edge] =
		for {
			p1 <- points
			p2 <- points
			if p1 != p2
			if p1.x < p2.x || (p1.x == p2.x && p1.y < p2.y) 
		} yield Edge(p1, p2)
		
	lazy val allEdges = generateEdges(allPoints) toList
			
	lazy val edgesIndices = allEdges.map(t => (t, allEdges.indexOf(t))) toMap

	lazy val allTriangles = 
		(for {
			p1 <- allPoints
			p2 <- allPoints
			p3 <- allPoints
			if p1 != p2
			if p1 != p3
			if p2 != p3
			if p1 < p2
			if p2 < p3
		} yield (p1, p2, p3)) flatMap(t => List(Triangle.getCanonicalTriangle(t._1, t._2, t._3, Player1), Triangle.getCanonicalTriangle(t._1, t._2, t._3, Player2)))
	
	lazy val trianglesIndices = allTriangles.map(t => (t, allTriangles.indexOf(t))) toMap
	
	/**
	 * Represents game board.
	 */
	class Board(val nextPlayer: Player, val points: List[Pos], val edges: List[Edge], val triangles: List[Triangle], val hashEdges: List[Int], val hashTriangles: List[Int], val parentBoard: Board) {
		/**
		 * Mutable state which keeps track of parent <-> child relationships.
		 * During pruning lots of nodes are removed from the solutions tree, which
		 * means a board might have some or all of its child boards removed. In such
		 * case we still need to understand what the "next generation" boards are, 
		 * and by far the easiest way to keep this info is to have mutable references.  
		 */
		var parents = if (parentBoard == null) List() else List(parentBoard)
		var children: List[Board] = List()

		/**
		 * Creates a new board by adding an edge to current board. 
		 */
		def extend(e: Edge): Board = {
			if (isValidEdge(e) == false) throw new Exception("invalid edge")
			else {
				def insert(l: List[Int], i: Int): List[Int] = {
					if (l.isEmpty) return List(i)
					else if (l.head >= i) i :: l
					else l.head :: insert(l.tail, i)
				}
				
				val newTriangles = pointsFormingTriangle(e).map(p1 => Triangle.getCanonicalTriangle(e.from, e.to, p1, nextPlayer))
				val newEdgesHash = insert(hashEdges, edgesIndices(e))
				val newTrianglesHash = newTriangles.map(t => trianglesIndices(t)) ::: hashTriangles
				
				new Board(if (newTriangles.isEmpty) { if (nextPlayer == Player1) Player2 else Player1 } else nextPlayer, if (newTriangles.isEmpty) points else points.filter(p => newTriangles.forall(t => !t.contains(p))), e :: edges, newTriangles ::: triangles, newEdgesHash, newTrianglesHash, this)
			}
		}
		
		/**
		 * Checks if a given edge can be added to current board.
		 */
		def isValidEdge(e: Edge): Boolean = points.forall(p => !e.contains(p)) && (if (edges.isEmpty) true else edges.forall(t => !t.intersect(e)))
		
		/**
		 * Checks if a given edge forms a triangle if placed on the board.
		 */
		def formsTriangle(e: Edge): Boolean = !pointsFormingTriangle(e).isEmpty
		
		def pointsFormingTriangle(e: Edge): List[Pos] = {
			def getPointReachableFromPoint(p: Pos) =
				(for {
					e1 <- edges
					if e1 != e
					if e1.from == p || e1.to == p
				} yield if (e1.from == p) e1.to else e1.from) toList
				
			val pointsReachableByFrom = getPointReachableFromPoint(e.from)
			val pointsReachableByTo = getPointReachableFromPoint(e.to)

			(for {
				p1 <- pointsReachableByFrom
				p2 <- pointsReachableByTo
				if p1 == p2
			} yield p1) toList
		}
		
		lazy val hasTwoSideTriangles = {
			val edgesFormingTriangles = nextEdges filter (t => formsTriangle(t))
			!edgesFormingTriangles.isEmpty
		}
		
		/**
		 * Gets all valid edges which can be placed on the board.
		 */
		lazy val nextEdges: List[Edge] = dim match {
			case BoardDimension(2, 2) if edges.isEmpty => 
				List(Edge(Pos(1, 1), Pos(1, 2)), Edge(Pos(1, 1), Pos(2, 2)))
				
			case BoardDimension(3, 3) if edges.isEmpty => 
				List(Edge(Pos(1, 1), Pos(1, 2)), Edge(Pos(1, 1), Pos(2, 2)), Edge(Pos(1, 1), Pos(2, 3)), Edge(Pos(1, 2), Pos(2, 1)))
			
			case _ => {
				val allEdges = generateEdges(points) filter(isValidEdge) toList
				val edgesFormingTriangles = allEdges filter (t => formsTriangle(t))
				if (edgesFormingTriangles.isEmpty) allEdges else edgesFormingTriangles
			}
		}
		
		/**
		 * Gets all boards which can be generated from current board.
		 */
		lazy val nextBoards: List[Board] = {
			val allNextBoards = nextEdges map extend
			val p = allNextBoards.partition(_.hasTwoSideTriangles)
			children = if (p._2.isEmpty) allNextBoards else p._2
			children
		}
		
		/**
		 * Gets scores - number of winning boards for each player.
		 */
		lazy val scores: Map[Player, Int] = {
			if (isCompleted) {
				if (player1Area > player2Area) List((Player1, 1), (Player2, 0), (PlayerUnknown, 0)) toMap
				else if (player2Area > player1Area) List((Player1, 0), (Player2, 1), (PlayerUnknown, 0)) toMap
				else List((Player1, 0), (Player2, 0), (PlayerUnknown, 1)) toMap
			} else {
				val f0: Map[Player, Int] = List((Player1, 0), (Player2, 0), (PlayerUnknown, 0)) toMap
				def merge(map1: Map[Player, Int], map2: Map[Player, Int]): Map[Player, Int] = 
					map1 ++ map2.map{ case (k,v) => k -> (v + map1.getOrElse(k,0)) }
				val res = children.map(t => t.scores).fold(f0)((res, item) => merge(res, item))
				res
			}
		}
		
		/**
		 * Gets area occupied by a given player.
		 */
		def getArea(p: Player) = 
			if (triangles.isEmpty) 0
			else math.ceil(100 * triangles.filter(t => t.player == p).map(t => t.area).sum / ((dim.width - 1) * (dim.height - 1)))
			
		lazy val player1Area: Double = getArea(Player1)

		lazy val player2Area: Double = getArea(Player2)
			
		def isCompleted: Boolean = nextEdges.isEmpty
		
		override def toString(): String = "Board: " + edges.toString
	}
	
	object Board {
		def Board(): Board = {
			new Board(Player1, allPoints, List(), List(), List(), List(), null)
		}
	}
	
	def onDebug(l: List[Board])(f: Board => Unit) {
		val res = l.filter(b => b.toString() == "Board: List(Edge(Pos(1,2),Pos(2,3)), Edge(Pos(1,1),Pos(2,2)))")
		if (!res.isEmpty)
			f(res.head)
	}
}