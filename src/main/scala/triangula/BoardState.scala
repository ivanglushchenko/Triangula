package triangula

case class BoardState(
		val points: List[Pos], 
		val edges: List[Edge], 
		val edgesIndices: Map[Edge, Int],
		val triangles: List[Triangle],
		val trianglesIndices: Map[Triangle, Int],
		val hashEdges: List[Int],
		val hashTriangles: List[Int]) {

	def extend(e: Edge): BoardState = {
		this
	}
	
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
}

object BoardState {
	def apply(dim: BoardDimension): BoardState = {
		val allPoints = 
			(for {
				x <- (1 to dim.width)
				y <- (1 to dim.height)
			} yield Pos(x, y)) toList
		val allEdges = Edge.generateFromPoints(allPoints)
		val allEdgesIndices = allEdges.map(t => (t, allEdges.indexOf(t))) toMap
		val allTriangles = 
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
		val allTrianglesIndices = allTriangles.map(t => (t, allTriangles.indexOf(t))) toMap
			
		new BoardState(
				allPoints, 
				allEdges,
				allEdgesIndices,
				allTriangles,
				allTrianglesIndices,
				List(),
				List())
	}
	
	def apply(points: List[Pos]): BoardState = {
		new BoardState(
				points, 
				List(),
				Map.empty,
				List(),
				Map.empty,
				List(),
				List())
	}
}