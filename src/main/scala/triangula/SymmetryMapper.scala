package triangula

import scala.collection.immutable._

trait SymmetryMapper extends BoardDefinition {
	/**
	 * Gets symmetrical edges according to a given mapping.
	 */
	def generateSymmetricEdgeMappings(allPointSymm: Map[Pos, Pos]): Map[Int, Int] =
		all.edges.map(p => {
			val s_from = allPointSymm(p.from)
			val s_to = allPointSymm(p.to)
			val s = if (all.edgesIndices.contains(Edge(s_from, s_to))) Edge(s_from, s_to) else Edge(s_to, s_from)
			
			if (p.isSame(s)) (all.edgesIndices(p), all.edgesIndices(p)) 
			else (all.edgesIndices(p), all.edgesIndices(s))
		}) toMap
	
	/**
	 * Gets symmetrical triangles according to a given mapping.
	 */
	def generateSymmetricTriangleMappings(allPointSymm: Map[Pos, Pos]): Map[Int, Int] =
		all.triangles.map(t => (all.trianglesIndices(t), all.trianglesIndices(t.map(allPointSymm)))) toMap

	/**
	 * These are all classes of symmetry the solver checks for during pruning.
	 */
	val symmetricMappers: List[Pos => Pos] = List(
	    p => p,
	    p => Pos(p.y, p.x),
	    p => Pos(dim.width - p.x + 1, p.y),
	    p => Pos(p.x, dim.height - p.y + 1),
	    p => Pos(dim.height - p.y + 1, dim.width - p.x + 1),
	    p => Pos(p.y, dim.width - p.x + 1),
	    p => Pos(dim.height - p.y + 1, p.x),
	    p => Pos(dim.width - p.x + 1, dim.height - p.y + 1))

	/**
	 * Mappings between symmetrical edges and triangles. 
	 */
	lazy val symmetries = symmetricMappers
		.map(f => all.points.map(p => (p, f(p))) toMap)
		.map(f => (generateSymmetricEdgeMappings(f), generateSymmetricTriangleMappings(f))) 

	/**
	 * Prunes symmetrical boards.
	 */
	def removeSymmetricalBoards(list: List[Board]): List[Board] = {
		/**
		 * Checks if one hash is smaller than the other.
		 */
		def isSmaller(l1: List[Int], l2: List[Int]): Boolean = {
			if (l1.isEmpty) false
			else {
				if (l1.head == l2.head) isSmaller(l1.tail, l2.tail)
				else l1.head < l2.head 
			}
		}
		
		/**
		 * Gets the smaller hash.
		 */
		def getLowerSymmHash(h1: List[Int], h2: List[Int], m1: Map[Int, Int], m2: Map[Int, Int]): (List[Int], List[Int]) = {
			val h1s = h1 map(t => m1(t)) sortBy(t => t)
			val h2s = h2 map(t => m2(t)) sortBy(t => t)
			val r1 = if (isSmaller(h1, h1s)) h1 else h1s
			val r2 = if (isSmaller(h2, h2s)) h2 else h2s
			(r1, r2)
		}
		
		/**
		 * Given mappings for edges and triangles, removes duplicates from boards list.
		 */
		def compactify(l: List[Board], mappings: (Map[Int, Int], Map[Int, Int])): List[Board] = {
			val sets = l groupBy(t => getLowerSymmHash(t.hashEdges, t.hashTriangles, mappings._1, mappings._2)) map(t => t._2.sortWith((b1, b2) => isSmaller(b1.hashEdges, b2.hashEdges))) toList
			
			for { symBoards <- sets; if !symBoards.tail.isEmpty } {
				for { boardToBeRemoved <- symBoards.tail} {
					symBoards.head.parents = boardToBeRemoved.parents ::: symBoards.head.parents
					for { affectedParent <- boardToBeRemoved.parents} {
						affectedParent.children = symBoards.head :: (affectedParent.children.filterNot(b => b == boardToBeRemoved))// - boardToBeRemoved)
					}
				}
			}
			
			sets map(t => t.head)
		}
		
		symmetries.foldLeft(list)((acc, mapping) => compactify(acc, mapping))
	}
}