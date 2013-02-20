package triangula

/**
 * Represents a hash for a given board. If two boards have the same hash, then 
 * there exists a mapping (associated with one of the supported types of symmetry)
 * which converts one board to another.
 */
case class BoardHash(val edges: List[Int], val triangles: List[Int]) {
  def addEdge(newEdge: Int): BoardHash = {
    def insertIndex(l: List[Int]): List[Int] = l match {
      case Nil => List(newEdge)
      case hd :: tl => if (hd >= newEdge) newEdge :: l else hd :: insertIndex(tl)
    }
    BoardHash(insertIndex(edges), triangles)
  }

  def addTriangles(newTriangles: List[Int]): BoardHash =
    BoardHash(edges, newTriangles ::: triangles)

  def <(t: BoardHash): Boolean = compare(edges, t.edges) match {
    case 0 => compare(triangles, t.triangles) < 0
    case n => n < 0
  }

  def compare(l1: List[Int], l2: List[Int]): Int = (l1, l2) match {
    case (Nil, _ :: _) => -1
    case (Nil, _) => 0
    case (hd1 :: tl1, hd2 :: tl2) =>
      if (hd1 < hd2) -1
      else if (hd1 > hd2) 1
      else compare(tl1, tl2)
  }

  /**
   * Checks if one list of indices is smaller than the other.
   */
  def isSmaller(l1: List[Int], l2: List[Int]): Boolean = compare(l1, l2) < 0

  /**
   * Maps this hash to a symmetrical representation.
   */
  def map(mappings: (Map[Int, Int], Map[Int, Int])): BoardHash = {
    def getSmallerHash(hash: List[Int], mapping: Map[Int, Int]): List[Int] = {
      val mappedHash = hash map (t => mapping(t)) sortBy (t => t)
      if (isSmaller(hash, mappedHash)) hash else mappedHash
    }
    BoardHash(getSmallerHash(edges, mappings._1), getSmallerHash(triangles, mappings._2))
  }
}

object BoardHash {
  def apply(): BoardHash = {
    BoardHash(List(), List())
  }
}