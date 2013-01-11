package triangula

import scala.collection.immutable._
import scala.collection.immutable.Map

trait GameDef {
	sealed abstract class Player
	case object Player1 extends Player {
		override def toString() = "x"
	}
	case object Player2 extends Player {
		override def toString() = "o"
	}
	case object PlayerUnknown extends Player {
		override def toString() = "?"
	}
	
	case class Pos(x: Int, y: Int){
		def -(t: Pos): (Int, Int) = (t.x - x, t.y - y)
		def <(t: Pos): Boolean = x < t.x || (x == t.x && y < t.y)
		def >(t: Pos): Boolean = x > t.x || (x == t.x && y > t.y)
	}
	
	case class Edge(from: Pos, to: Pos){
		def intersect(e: Edge): Boolean = 
			if ((from == e.from && to == e.to) || (from == e.to && to == e.from)) true
			else if ((from == e.from && to != e.to) || (from != e.from && to == e.to) || (from == e.to && to != e.from) || (from != e.to && to == e.from)) false
			else {
				if (to.x != from.x) {
					val t2 = ((e.from.y - from.y) * (to.x - from.x) - (e.from.x - from.x) * (to.y - from.y)).toFloat / ((e.to.x - e.from.x) * (to.y - from.y) - (e.to.y - e.from.y) * (to.x - from.x)).toFloat
					val t1 = ((e.to.x - e.from.x) * t2 + e.from.x - from.x).toFloat / (to.x - from.x).toFloat
					t2 >=0 && t2 <= 1 && t1 >= 0 && t1 <= 1
				}
				else {
					val t2 = ((e.from.x - from.x) * (to.y - from.y) - (e.from.y - from.y) * (to.x - from.x)).toFloat / ((e.to.y - e.from.y) * (to.x - from.x) - (e.to.x - e.from.x) * (to.y - from.y)).toFloat
					val t1 = ((e.to.y - e.from.y) * t2 + e.from.y - from.y).toFloat / (to.y - from.y).toFloat
					t2 >=0 && t2 <= 1 && t1 >= 0 && t1 <= 1
				}
			}
		
		def contains(p: Pos): Boolean =
			if (p == from || p == to) false
			else {
				if (to.x != from.x) {
					val t = (p.x - from.x).toFloat / (to.x - from.x).toFloat
					if (t > 1 || t < 0) false
					else {
						val y = (to.y - from.y) * t + from.y
						math.abs(y - p.y) < 0.00001
					}
				} else {
					val t = (p.y - from.y).toFloat / (to.y - from.y).toFloat
					if (t > 1 || t < 0) false
					else {
						val x = (to.x - from.x) * t + from.x
						if (math.abs(x - p.x) < 0.00001) true
						else false
					}
				}
			}
		
		def isSame(e: Edge) = (from == e.from && to == e.to) || (from == e.to && to == e.from)
	}
	
	case class Triangle(p1: Pos, p2: Pos, p3: Pos, player: Player){
		val area = {
			def length(p1: Pos, p2: Pos): Double = math.sqrt((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y))
			val l1 = length(p1, p2)
			val l2 = length(p2, p3)
			val l3 = length(p1, p3)
			val s = (l1 + l2 + l3) / 2.0
			math.sqrt(s * (s - l1) * (s - l2) * (s - l3))
		}
		
		def dot(v1: (Int, Int), v2: (Int, Int)): Double = v1._1 * v2._1 + v1._2 * v2._2
		
		def contains(p: Pos): Boolean = {
			val v0 = p2 - p1
			val v1 = p3 - p1
			val v2 = p  - p1
			val dot00 = dot(v0, v0)
			val dot01 = dot(v0, v1)
			val dot02 = dot(v0, v2)
			val dot11 = dot(v1, v1)
			val dot12 = dot(v1, v2)			
			val invDenom = 1 / (dot00 * dot11 - dot01 * dot01)
			val u = (dot11 * dot02 - dot01 * dot12) * invDenom
			val v = (dot00 * dot12 - dot01 * dot02) * invDenom
			(u > 0) && (v > 0) && (u + v < 1)
		}
	}
	
	type Map = Pos => Player
	
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
		
	lazy val edgesIndicesSymm0 = generateSymmetricEdgeMappings(p => p)
	lazy val edgesIndicesSymm1 = generateSymmetricEdgeMappings(p => Pos(p.y, p.x))
	lazy val edgesIndicesSymm2 = generateSymmetricEdgeMappings(p => Pos(width - p.x + 1, p.y))
	lazy val edgesIndicesSymm3 = generateSymmetricEdgeMappings(p => Pos(p.x, height - p.y + 1))
	lazy val edgesIndicesSymm4 = generateSymmetricEdgeMappings(p => Pos(height - p.y + 1, width - p.x + 1))
	lazy val edgesIndicesSymm5 = generateSymmetricEdgeMappings(p => Pos(p.y, width - p.x + 1))
	lazy val edgesIndicesSymm6 = generateSymmetricEdgeMappings(p => Pos(height - p.y + 1, p.x))
	lazy val edgesIndicesSymm7 = generateSymmetricEdgeMappings(p => Pos(width - p.x + 1, height - p.y + 1))
		
	lazy val trianglesIndicesSymm0 = generateSymmetricTriangleMappings(p => p)
	lazy val trianglesIndicesSymm1 = generateSymmetricTriangleMappings(p => Pos(p.y, p.x))
	lazy val trianglesIndicesSymm2 = generateSymmetricTriangleMappings(p => Pos(width - p.x + 1, p.y))
	lazy val trianglesIndicesSymm3 = generateSymmetricTriangleMappings(p => Pos(p.x, height - p.y + 1))
	lazy val trianglesIndicesSymm4 = generateSymmetricTriangleMappings(p => Pos(height - p.y + 1, width - p.x + 1))
	lazy val trianglesIndicesSymm5 = generateSymmetricTriangleMappings(p => Pos(p.y, width - p.x + 1))
	lazy val trianglesIndicesSymm6 = generateSymmetricTriangleMappings(p => Pos(height - p.y + 1, p.x))
	lazy val trianglesIndicesSymm7 = generateSymmetricTriangleMappings(p => Pos(width - p.x + 1, height - p.y + 1))
	
	lazy val symmetries = List(
			(edgesIndicesSymm0, trianglesIndicesSymm0),
			(edgesIndicesSymm1, trianglesIndicesSymm1),
			(edgesIndicesSymm2, trianglesIndicesSymm2),
			(edgesIndicesSymm3, trianglesIndicesSymm3),
			(edgesIndicesSymm4, trianglesIndicesSymm4),
			(edgesIndicesSymm5, trianglesIndicesSymm5),
			(edgesIndicesSymm6, trianglesIndicesSymm6),
			(edgesIndicesSymm7, trianglesIndicesSymm7))
			
	def onDebug(l: List[Board])(f: Board => Unit) {
		val res = l.filter(b => b.toString() == "Board: List(Edge(Pos(1,2),Pos(2,3)), Edge(Pos(1,1),Pos(2,2)))")
		if (!res.isEmpty)
			f(res.head)
	}
	
	class Board(val nextPlayer: Player, val points: List[Pos], val edges: List[Edge], val triangles: List[Triangle], val hashEdges: List[Int], val hashTriangles: List[Int], val parentBoard: Board) {
		var parents = if (parentBoard == null) List() else List(parentBoard)
		var children: List[Board] = List()

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
		
		def isValidEdge(e: Edge): Boolean = points.forall(p => !e.contains(p)) && (if (edges.isEmpty) true else edges.forall(t => !t.intersect(e)))
		
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
	
	val startingBoard: Board
}