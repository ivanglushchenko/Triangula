package triangula

import scala.collection.immutable._
import scala.collection.immutable.Map

/**
 * Contains game components/logic.
 */
trait GameDef {
	val width: Int
	val height: Int
	
	lazy val allPoints = 
		(for {
			x <- (1 to width)
			y <- (1 to height)
		} yield Pos(x, y)) toList
		
	lazy val allEdges = 
		(for {
					p1 <- allPoints
					p2 <- allPoints
					if p1 != p2
					if p1.x < p2.x || (p1.x == p2.x && p1.y < p2.y) 
					e = Edge(p1, p2)
			} yield e) toList
			
	lazy val edgesIndices = allEdges.map(t => (t, allEdges.indexOf(t))) toMap
	
	/**
	 * Gets a "canonical" representation of a triangle. The idea: given 3 points A,B,C you
	 * can define a triangle in 6 different ways: ABC, ACB,  CBA, and so on. This function
	 * returns triangle ABC for any of the combinations of points.  
	 */
	def getTriangle(p1: Pos, p2: Pos, p3:Pos, p: Player): Triangle = {
		val s1 = 
			if (p1 < p2 && p1 < p3) p1
			else if (p2 < p1 && p2 < p3) p2
			else p3
		val s3 = 
			if (p1 > p2 && p1 > p3) p1
			else if (p2 > p1 && p2 > p3) p2
			else p3		
		val s2 = 
			if (p1 != s1 && p1 != s3) p1
			else if (p2 != s1 && p2 != s3) p2
			else p3
		if (s1 == s2 || s1 == s3 || s2 == s3) throw new Exception("bad triangle")
		new Triangle(s1, s2, s3, p)
	}
	
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
		} yield (p1, p2, p3)) flatMap(t => List(getTriangle(t._1, t._2, t._3, Player1), getTriangle(t._1, t._2, t._3, Player2)))
	
	lazy val trianglesIndices = allTriangles.map(t => (t, allTriangles.indexOf(t))) toMap
	
	/**
	 * Gets symmetrical edges according to a given mapping.
	 */
	def generateSymmetricEdgeMappings(symmetryMapping: Pos => Pos): scala.collection.immutable.Map[Int, Int] = {
		val allPointSymm = allPoints.map(p => (p, symmetryMapping(p))) toMap
		val allEdgesSymm = allEdges.map(p => {
			val s_from = allPointSymm(p.from)
			val s_to = allPointSymm(p.to)
			val s = if (edgesIndices.contains(Edge(s_from, s_to))) Edge(s_from, s_to) else Edge(s_to, s_from)
			if (p.isSame(s))
				(p, p)
			else
				(p, s)
		}) toMap
		
		allEdgesSymm.map(t => (edgesIndices(t._1), edgesIndices(t._2)))
	}
	
	/**
	 * Gets symmetrical triangles according to a given mapping.
	 */
	def generateSymmetricTriangleMappings(symmetryMapping: Pos => Pos): scala.collection.immutable.Map[Int, Int] = {
		val allPointSymm = allPoints.map(p => (p, symmetryMapping(p))) toMap
		val indices = allTriangles.map(t => {
			val s_p1 = allPointSymm(t.p1)
			val s_p2 = allPointSymm(t.p2)
			val s_p3 = allPointSymm(t.p3)
			val s = getTriangle(s_p1, s_p2, s_p3, t.player)
			(trianglesIndices(t), trianglesIndices(s))
		})
		
		indices toMap
	}
	
	/**
	 * These are all classes of symmetry the solver checks for during pruning.
	 */
	lazy val symmetries = List(
			(generateSymmetricEdgeMappings(p => p),                                      generateSymmetricTriangleMappings(p => p)),
			(generateSymmetricEdgeMappings(p => Pos(p.y, p.x)),                          generateSymmetricTriangleMappings(p => Pos(p.y, p.x))),
			(generateSymmetricEdgeMappings(p => Pos(width - p.x + 1, p.y)),              generateSymmetricTriangleMappings(p => Pos(width - p.x + 1, p.y))),
			(generateSymmetricEdgeMappings(p => Pos(p.x, height - p.y + 1)),             generateSymmetricTriangleMappings(p => Pos(p.x, height - p.y + 1))),
			(generateSymmetricEdgeMappings(p => Pos(height - p.y + 1, width - p.x + 1)), generateSymmetricTriangleMappings(p => Pos(height - p.y + 1, width - p.x + 1))),
			(generateSymmetricEdgeMappings(p => Pos(p.y, width - p.x + 1)),              generateSymmetricTriangleMappings(p => Pos(p.y, width - p.x + 1))),
			(generateSymmetricEdgeMappings(p => Pos(height - p.y + 1, p.x)),             generateSymmetricTriangleMappings(p => Pos(height - p.y + 1, p.x))),
			(generateSymmetricEdgeMappings(p => Pos(width - p.x + 1, height - p.y + 1)), generateSymmetricTriangleMappings(p => Pos(width - p.x + 1, height - p.y + 1))))
	
	/**
	 * Represents game board.
	 */
	class Board(val nextPlayer: Player, val points: List[Pos], val edges: List[Edge], val triangles: List[Triangle], val hashEdges: List[Int], val hashTriangles: List[Int], val parentBoard: Board) {
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
				
				val newTriangles = for {
					p1 <- pointsFormingTriangle(e)
				} yield getTriangle(e.from, e.to, p1, nextPlayer)
				
				val newEdgesHash = insert(hashEdges, edgesIndices(e))
				val newTrianglesHash =
					newTriangles.map(t => trianglesIndices(t)) ::: hashTriangles
				
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
			val pointsReachableByFrom = 
				(for {
					e1 <- edges
					if e1 != e
					if e1.from == e.from || e1.to == e.from
				} yield if (e1.from == e.from) e1.to else e1.from) toList
			val pointsReachableByTo = 
				(for {
					e1 <- edges
					if e1 != e
					if e1.from == e.to || e1.to == e.to
				} yield if (e1.from == e.to) e1.to else e1.from) toList
				
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
		lazy val nextEdges: List[Edge] = {
			// for empty boards the set of open edges is reduced because of different types of symmetries
			if (width == 2 && height == 2 && edges.isEmpty) List(Edge(Pos(1, 1), Pos(1, 2)), Edge(Pos(1, 1), Pos(2, 2)))
			else if (width == 3 && height == 3 && edges.isEmpty) List(Edge(Pos(1, 1), Pos(1, 2)), Edge(Pos(1, 1), Pos(2, 2)), Edge(Pos(1, 1), Pos(2, 3)), Edge(Pos(1, 2), Pos(2, 1)))
			else {
				val allEdges = 
					(for {
								p1 <- points
								p2 <- points
								if p1 != p2
								if p1.x < p2.x || (p1.x == p2.x && p1.y < p2.y) 
								e = Edge(p1, p2)
								if isValidEdge(e)
						} yield e) toList
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
			children =
				if (p._2.isEmpty)
					allNextBoards
				else
					p._2
			children
		}
		
		/**
		 * Gets scores - number of winning boards for each player.
		 */
		lazy val scores: scala.collection.immutable.Map[Player, Int] = {
			if (isCompleted) {
				if (player1Area > player2Area) List((Player1, 1), (Player2, 0), (PlayerUnknown, 0)) toMap
				else if (player2Area > player1Area) List((Player1, 0), (Player2, 1), (PlayerUnknown, 0)) toMap
				else List((Player1, 0), (Player2, 0), (PlayerUnknown, 1)) toMap
			} else {
				val f0: scala.collection.immutable.Map[Player, Int] = List((Player1, 0), (Player2, 0), (PlayerUnknown, 0)) toMap
				def merge(map1: scala.collection.immutable.Map[Player, Int], map2: scala.collection.immutable.Map[Player, Int]): scala.collection.immutable.Map[Player, Int] = 
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
			else math.ceil(100 * triangles.filter(t => t.player == p).map(t => t.area).sum / ((width - 1) * (height - 1)))
			
		lazy val player1Area: Double = getArea(Player1)

		lazy val player2Area: Double = getArea(Player2)
			
		def isCompleted: Boolean = nextEdges.isEmpty
		
		override def toString(): String =
			"Board: " + edges.toString
			//"\nBoard:\n\tEdges: " + edges.toString + "\n\tTriangles: " + triangles.toString + "\n\tPoints: " + points.toString
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
	
	val startingBoard: Board
	
	//val allCompletedBoards: List[Board]
}