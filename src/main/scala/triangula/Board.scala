package triangula

/**
 * Represents game board.
 */
class Board(
  val definition: BoardDefinition,
  val player: Player,
  val points: List[Pos],
  val edges: List[Edge],
  val triangles: List[Triangle],
  val hash: BoardHash,
  val parentBoard: Board) {

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
  def extend(e: Edge): Board =
    if (isValidEdge(e) == false) throw new Exception("invalid edge")
    else {
      val newTriangles = pointsFormingTriangle(e).map(Triangle.getCanonicalTriangle(e.from, e.to, _, player))
      // Create a new board with respect to game's rules
      new Board(
        definition,
        if (newTriangles.isEmpty) player.next() else player,
        if (newTriangles.isEmpty) points else points.filter(p => newTriangles.forall(t => !t.contains(p))),
        e :: edges,
        newTriangles ::: triangles,
        hash.addEdge(definition.allEdgesIndices(e)).addTriangles(newTriangles.map(definition.allTrianglesIndices(_))),
        this)
    }

  /**
   * Checks if a given edge can be added to current board.
   */
  def isValidEdge(e: Edge): Boolean = points.forall(!e.contains(_)) && (if (edges.isEmpty) true else edges.forall(!_.intersect(e)))

  /**
   * Checks if a given edge forms a triangle if placed on the board.
   */
  def formsTriangle(e: Edge): Boolean = !pointsFormingTriangle(e).isEmpty

  /**
   * Gets points which form a triangle with the given edge.
   */
  def pointsFormingTriangle(e: Edge): List[Pos] = {
    def getPointReachableFromPoint(p: Pos): List[Pos] =
      for (e1 <- edges; if e1 != e; if e1.from == p || e1.to == p)
      yield if (e1.from == p) e1.to else e1.from

    val pointsReachableByFrom = getPointReachableFromPoint(e.from)
    val pointsReachableByTo = getPointReachableFromPoint(e.to)

    for {
      p1 <- pointsReachableByFrom
      p2 <- pointsReachableByTo
      if p1 == p2
    } yield p1
  }

  lazy val hasTwoSideTriangles = !nextEdges.filter(t => formsTriangle(t)).isEmpty

  /**
   * Gets all valid edges which can be placed on the board.
   */
  lazy val nextEdges: List[Edge] = definition.dim match {
    case BoardDimension(2, 2) if edges.isEmpty =>
      List(Edge(Pos(1, 1), Pos(1, 2)), Edge(Pos(1, 1), Pos(2, 2)))

    case BoardDimension(3, 3) if edges.isEmpty =>
      List(Edge(Pos(1, 1), Pos(1, 2)), Edge(Pos(1, 1), Pos(2, 2)), Edge(Pos(1, 1), Pos(2, 3)), Edge(Pos(1, 2), Pos(2, 1)))

    case _ => {
      val allEdges = Edge fromPoints points filter isValidEdge toList
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
   * Gets scores - number of winning boards for each player and other statistics.
   */
  lazy val score = Score(this)

  def isCompleted: Boolean = nextEdges.isEmpty

  override def toString(): String = "Board: " + edges.toString
}

object Board {
  def apply(definition: BoardDefinition): Board = {
    new Board(
      definition,
      Player1,
      definition.allPoints,
      List(),
      List(),
      BoardHash(),
      null)
  }
}