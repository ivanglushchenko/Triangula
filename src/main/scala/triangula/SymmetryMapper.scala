package triangula

import scala.collection.immutable._

sealed abstract class Symmetry
case object Identity extends Symmetry
case object Diagonal extends Symmetry
case object FlipHorizontally extends Symmetry
case object FlipVertically extends Symmetry
case class Composite(symmetries: List[Symmetry]) extends Symmetry

trait SymmetryMapper extends BoardDefinition {
	def getMapper(symm: Symmetry): Pos => Pos = symm match {
		case Identity         => p => p
		case Diagonal         => p => Pos(p.y, p.x)
		case FlipVertically   => p => Pos(dim.width - p.x + 1, p.y)
		case FlipHorizontally => p => Pos(p.x, dim.height - p.y + 1)
		case Composite(Nil)   => p => p
		case Composite(list)  => list.foldLeft(getMapper(Identity))((acc, s) => acc.compose(getMapper(s)))
	}
	
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
	 * Gets symmetrical edges according to a given mapping.
	 */
	def generateSymmetricEdgeMappings(allPointSymm: Map[Pos, Pos]): Map[Int, Int] =
		allEdges.map(p => {
			val s_from = allPointSymm(p.from)
			val s_to = allPointSymm(p.to)
			val s = if (allEdgesIndices.contains(Edge(s_from, s_to))) Edge(s_from, s_to) else Edge(s_to, s_from)
			
			if (p.isSame(s)) (allEdgesIndices(p), allEdgesIndices(p)) 
			else (allEdgesIndices(p), allEdgesIndices(s))
		}) toMap
	
	/**
	 * Gets symmetrical triangles according to a given mapping.
	 */
	def generateSymmetricTriangleMappings(allPointSymm: Map[Pos, Pos]): Map[Int, Int] =
		allTriangles.map(t => (allTrianglesIndices(t), allTrianglesIndices(t.map(allPointSymm)))) toMap

	/**
	 * Mappings between symmetrical edges and triangles. 
	 */
	lazy val symmetries = symmetricMappers
		.map(f => allPoints.map(p => (p, f(p))) toMap)
		.map(f => (generateSymmetricEdgeMappings(f), generateSymmetricTriangleMappings(f))) 

	/**
	 * Prunes symmetrical boards.
	 */
	def removeSymmetricalBoards(list: List[Board]): List[Board] = {
		/**
		 * Given mappings for edges and triangles, removes duplicates from boards list.
		 */
		def compactify(l: List[Board], mappings: (Map[Int, Int], Map[Int, Int])): List[Board] = {
			val sets = l groupBy(t => t.hash.map(mappings)) map(t => t._2.sortWith((b1, b2) => b1.hash < b2.hash)) toList
			
			for { symBoards <- sets; if !symBoards.tail.isEmpty } {
				for { boardToBeRemoved <- symBoards.tail} {
					symBoards.head.parents = boardToBeRemoved.parents ::: symBoards.head.parents
					for { affectedParent <- boardToBeRemoved.parents} {
						affectedParent.children = symBoards.head :: (affectedParent.children.filterNot(b => b == boardToBeRemoved))
					}
				}
			}
			
			sets map(t => t.head)
		}
		
		symmetries.foldLeft(list)((acc, mapping) => compactify(acc, mapping))
	}
}