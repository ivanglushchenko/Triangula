package triangula

import org.scalatest.FunSuite
import SolverForBoard2By2._

class Board2By2SolverSuite extends FunSuite {
  def testMe(testName: String)(testFun: => Unit): Unit = test("2x2 board: " + testName)(testFun)
  
  testMe("number of edges = 6") {
    assert(allEdges.length == 6)
  }
  
  testMe("check edges") {
	assert(allEdges.toString == "List(Edge(Pos(1,1),Pos(1,2)), Edge(Pos(1,1),Pos(2,1)), Edge(Pos(1,1),Pos(2,2)), Edge(Pos(1,2),Pos(2,1)), Edge(Pos(1,2),Pos(2,2)), Edge(Pos(2,1),Pos(2,2)))")
  }
  
  testMe("check edges indices") {
	assert(allEdgesIndices.toString == "Map(Edge(Pos(2,1),Pos(2,2)) -> 5, Edge(Pos(1,1),Pos(2,1)) -> 1, Edge(Pos(1,2),Pos(2,2)) -> 4, Edge(Pos(1,1),Pos(1,2)) -> 0, Edge(Pos(1,2),Pos(2,1)) -> 3, Edge(Pos(1,1),Pos(2,2)) -> 2)")
  }
  
  testMe("extend") {
	val s1 = startingBoard extend (Edge(Pos(1, 1), Pos(1, 2)))
    assert(s1.nextEdges.length == 5)
  }
  
  testMe("extend multiple times") {
	val s2 = startingBoard extend (Edge(Pos(1, 1), Pos(2, 2)))
    assert(s2.nextEdges.length == 4)
    val s3 = s2 extend (Edge(Pos(1, 1), Pos(1, 2))) extend (Edge(Pos(1, 1), Pos(2, 1)))
    assert(s3.nextEdges.length == 2)
    val s4 = s3 extend (Edge(Pos(2, 1), Pos(2, 2))) extend (Edge(Pos(1, 2), Pos(2, 2)))
    assert(s4.nextEdges.length == 0)
  }
  
  testMe("check intersect") {
    assert(Edge(Pos(0, 0), Pos(2, 2)).intersect(Edge(Pos(2, 0), Pos(0, 2))) == true)
    assert(Edge(Pos(0, 0), Pos(2, 2)).intersect(Edge(Pos(2, 2), Pos(3, 10))) == false)
    assert(Edge(Pos(0, 0), Pos(2, 2)).intersect(Edge(Pos(0, 0), Pos(0, 2))) == false)
    assert(Edge(Pos(0, 0), Pos(2, 2)).intersect(Edge(Pos(0, 0), Pos(2, 0))) == false)
    assert(Edge(Pos(0, 0), Pos(2, 2)).contains(Pos(1, 1)) == true)
    assert(Edge(Pos(0, 0), Pos(2, 2)).contains(Pos(2, 1)) == false)
    assert(Edge(Pos(0, 0), Pos(2, 2)).contains(Pos(0, 0)) == false)
    assert(Edge(Pos(0, 0), Pos(2, 2)).contains(Pos(2, 2)) == false)
  }
  
  testMe("check extend() result") {
    val b = startingBoard extend (Edge(Pos(1, 1), Pos(1, 2))) extend (Edge(Pos(1, 1), Pos(2, 2)))
    assert(b.nextEdges == List(Edge(Pos(1, 2), Pos(2, 2))))
  }
  
  testMe("check isCompleted()") {
    val b = startingBoard extend (Edge(Pos(1, 1), Pos(1, 2))) extend (Edge(Pos(1, 1), Pos(2, 2))) extend (Edge(Pos(1, 2), Pos(2, 2))) extend (Edge(Pos(1, 1), Pos(2, 1))) extend (Edge(Pos(2, 1), Pos(2, 2)))
    assert(b.isCompleted, true)
  }
  
  testMe("check 2 first items of the board stream") {
    val l = ((for (board <- startingBoard.nextBoards; nextBoard <- board.nextBoards) yield nextBoard) toList).length
    assert(l == 5)
  }
  
  testMe("check nextboards") {
    assert(startingBoard.nextBoards.length == 2)
    assert(startingBoard.nextBoards.toString() == "List(Board: List(Edge(Pos(1,1),Pos(1,2))), Board: List(Edge(Pos(1,1),Pos(2,2))))")
  }
  
  testMe("check nextboards 2") {
    val l2Boards = nextBoards(startingBoard.nextBoards)
    assert(l2Boards.length == 2)
    assert(l2Boards.toString() == "List(Board: List(Edge(Pos(1,1),Pos(1,2)), Edge(Pos(1,1),Pos(2,2))), Board: List(Edge(Pos(2,1),Pos(2,2)), Edge(Pos(1,1),Pos(1,2))))")
    assert(l2Boards(0).hash.edges == List(0, 2))
    assert(l2Boards(1).hash.edges == List(0, 5))
    
    val l3BoardsForL2_1 = nextBoards(List(l2Boards(1)))
    assert(l3BoardsForL2_1.length == 2)
    assert(l3BoardsForL2_1(0).hash.edges == List(0, 1, 5))
    assert(l3BoardsForL2_1(1).hash.edges == List(0, 2, 5))
    
    assert(nextBoards(l2Boards).length == 3)
  }
  
  testMe("check canonical triangles") {
    assert(allTrianglesIndices(Triangle.getCanonicalTriangle(Pos(2, 1), Pos(1, 1), Pos(1, 2), Player1)) == 0)
    assert(allTrianglesIndices(Triangle.getCanonicalTriangle(Pos(2, 1), Pos(1, 2), Pos(1, 1), Player1)) == 0)
    assert(allTrianglesIndices(Triangle.getCanonicalTriangle(Pos(2, 1), Pos(1, 1), Pos(1, 2), Player2)) == 1)
    assert(allTrianglesIndices(Triangle.getCanonicalTriangle(Pos(1, 2), Pos(2, 1), Pos(1, 1), Player2)) == 1)
    assert(allTrianglesIndices(Triangle.getCanonicalTriangle(Pos(1, 1), Pos(1, 2), Pos(2, 1), Player2)) == 1)
  }
  
  testMe("completed boards") {
    assert(allCompletedBoards.length == 2)
  }
}
