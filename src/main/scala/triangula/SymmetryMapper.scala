package triangula

import scala.collection.immutable._

/**
 * These are basic types of symmetry Solver knows about.  
 */
sealed abstract class Symmetry
case object Identity extends Symmetry
case object RotateDiagonally extends Symmetry
case object RotateHorizontally extends Symmetry
case object RotateVertically extends Symmetry
case class Composite(symmetries: List[Symmetry]) extends Symmetry

object Composite {
  def apply(s1: Symmetry, s2: Symmetry): Composite = Composite(List(s1, s2))
  def apply(s1: Symmetry, s2: Symmetry, s3: Symmetry): Composite = Composite(List(s1, s2, s3))
}

trait SymmetryMapper extends BoardDefinition {
  /**
   * Converts a symmetry to a mapping function.
   */
  def getMapper(symm: Symmetry): Pos => Pos = symm match {
    case Identity =>           p => p
    case RotateDiagonally =>   p => Pos(p.y, p.x)
    case RotateHorizontally => p => Pos(p.x, dim.height - p.y + 1)
    case RotateVertically =>   p => Pos(dim.width - p.x + 1, p.y)
    case Composite(Nil) =>     p => p
    case Composite(list) => list.foldLeft(getMapper(Identity))((acc, s) => acc.compose(getMapper(s)))
  }

  /**
   * These are all classes of symmetry the solver checks for during pruning.
   */
  val symmetricMappers = List(
    Identity,
    RotateDiagonally,
    RotateHorizontally,
    RotateVertically,
    Composite(RotateHorizontally, RotateVertically),
    Composite(RotateDiagonally, RotateHorizontally),
    Composite(RotateDiagonally, RotateVertically),
    Composite(RotateDiagonally, RotateHorizontally, RotateVertically))
    .map(getMapper)

  /**
   * Gets symmetrical edges according to a given mapping.
   */
  def generateSymmetricEdgeMappings(allPointSymm: Map[Pos, Pos]): Map[Int, Int] = {
    def mapEdge(e: Edge) = {
      val mappedEdge = Edge(allPointSymm(e.from), allPointSymm(e.to))
      if (allEdgesIndices.contains(mappedEdge)) mappedEdge else mappedEdge.reverse
    }
    
    allEdges.map(p => {
      val s = mapEdge(p)
      (allEdgesIndices(p), if (p.isSame(s)) allEdgesIndices(p) else allEdgesIndices(s))
    }) toMap
  }

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
      val sets = l groupBy (t => t.hash.map(mappings)) map (t => t._2.sortWith(_.hash < _.hash)) toList

      // if we prune some boards, we have to update parent <-> child relationships
      for { symBoards <- sets; if !symBoards.tail.isEmpty } {
        for { boardToBeRemoved <- symBoards.tail } {
          symBoards.head.parents = boardToBeRemoved.parents ::: symBoards.head.parents
          for { affectedParent <- boardToBeRemoved.parents } {
            affectedParent.children = symBoards.head :: (affectedParent.children.filterNot(b => b == boardToBeRemoved))
          }
        }
      }

      sets map (_.head)
    }

    symmetries.foldLeft(list)((acc, mapping) => compactify(acc, mapping))
  }
}