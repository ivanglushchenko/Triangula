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

  lazy val allEdges = Edge fromPoints allPoints

  lazy val allEdgesIndices = allEdges.map(t => (t, allEdges.indexOf(t))) toMap

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
    } yield (p1, p2, p3)) flatMap (t => List(Triangle.getCanonicalTriangle(t._1, t._2, t._3, Player1), Triangle.getCanonicalTriangle(t._1, t._2, t._3, Player2)))
    
  lazy val allTrianglesIndices = allTriangles.map(t => (t, allTriangles.indexOf(t))) toMap
}