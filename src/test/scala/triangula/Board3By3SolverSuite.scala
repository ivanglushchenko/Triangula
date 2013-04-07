package triangula

import org.scalatest.FunSuite
import SolverForBoard3By3._

class Board3By3SolverSuite extends FunSuite {
  def testMe(testName: String)(testFun: => Unit): Unit = test("3x3 board: " + testName)(testFun)
  
  testMe("edges intersection") {
    val e1 = Edge(Pos(1, 1), Pos(1, 2))
    val e2 = Edge(Pos(2, 1), Pos(2, 2))
    val e3 = Edge(Pos(1, 1), Pos(3, 2))
    assert(e1.intersect(e3) === false)
    assert(e2.intersect(e3) === true)
  }
  
  testMe("num of points") {
    assert(startingBoard.points.length === 9)
  }
  
  testMe("valid edge") {
    val b1 = startingBoard extend (Edge(Pos(1, 1), Pos(2, 3)))
    assert(b1.isValidEdge(Edge(Pos(1, 3), Pos(2, 1))) === false)
    assert(b1.isValidEdge(Edge(Pos(1, 1), Pos(2, 3))) === false)
    assert(b1.isValidEdge(Edge(Pos(1, 1), Pos(2, 1))) === true)
    assert(b1.isValidEdge(Edge(Pos(1, 1), Pos(2, 2))) === true)
    assert(b1.isValidEdge(Edge(Pos(1, 1), Pos(3, 3))) === false)
    assert(b1.isValidEdge(Edge(Pos(1, 2), Pos(2, 3))) === true)

    val b2 = b1 extend (Edge(Pos(1, 2), Pos(2, 3)))
    assert(b2.isValidEdge(Edge(Pos(1, 2), Pos(2, 3))) === false)
    assert(b2.isValidEdge(Edge(Pos(1, 2), Pos(1, 1))) === true)
  }
  
  testMe("forms triangle") {
    val b1 = startingBoard extend (Edge(Pos(1, 1), Pos(2, 3)))
    val b2 = b1 extend (Edge(Pos(1, 2), Pos(2, 3)))
    assert(b2.formsTriangle(Edge(Pos(1, 1), Pos(1, 2))) === true)
    assert(b2.formsTriangle(Edge(Pos(1, 1), Pos(2, 1))) === false)
  }
  
  testMe("triangles") {
    val b3 = startingBoard extend (Edge(Pos(1, 2), Pos(3, 3))) extend (Edge(Pos(2, 1), Pos(3, 3))) extend (Edge(Pos(1, 2), Pos(2, 1)))
    assert(b3.triangles.head.contains(Pos(2, 2)) === true)
    assert(b3.triangles.head.contains(Pos(2, 1)) === false)
    assert(b3.triangles.head.contains(Pos(1, 2)) === false)
    assert(b3.triangles.head.contains(Pos(3, 3)) === false)
    assert(b3.triangles.head.contains(Pos(1, 1)) === false)
    assert(b3.triangles.head.contains(Pos(3, 1)) === false)
    assert(b3.points.length === 8)
  }
  
  testMe("nextBoards") {
    assert(startingBoard.nextBoards.length === 4)
    assert(startingBoard.nextBoards.toString === "List(Board: List(Edge(Pos(1,1),Pos(1,2))), Board: List(Edge(Pos(1,1),Pos(2,2))), Board: List(Edge(Pos(1,1),Pos(2,3))), Board: List(Edge(Pos(1,2),Pos(2,1))))")
  }
  
  testMe("extend board") {
    val b4 = startingBoard extend Edge(Pos(3, 2), Pos(3, 3)) extend Edge(Pos(2, 3), Pos(3, 3)) extend Edge(Pos(1, 1), Pos(2, 1)) extend Edge(Pos(1, 1), Pos(3, 2)) extend Edge(Pos(1, 1), Pos(2, 3)) extend Edge(Pos(1, 2), Pos(2, 3)) extend Edge(Pos(1, 3), Pos(2, 3)) extend Edge(Pos(3, 1), Pos(3, 2)) extend Edge(Pos(2, 1), Pos(3, 2)) extend Edge(Pos(2, 3), Pos(3, 2)) extend Edge(Pos(2, 1), Pos(3, 1)) extend Edge(Pos(1, 2), Pos(1, 3)) extend Edge(Pos(1, 1), Pos(1, 2))
    assert(b4.nextEdges.length === 0)
  }
  
  testMe("remove symmetrical boards") {
    val b5 = startingBoard extend Edge(Pos(1, 1), Pos(2, 2)) extend Edge(Pos(1, 2), Pos(2, 3))
    val b6 = startingBoard extend Edge(Pos(1, 2), Pos(2, 3)) extend Edge(Pos(1, 1), Pos(2, 2))
    val rem = removeSymmetricalBoards(List(b5, b6))
    assert(rem.length === 1)
  }
}