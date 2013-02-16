package triangula

import swing._

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel}
import scala.swing.event._
import java.awt.{Color, Graphics2D, Point, geom}

object Program extends SimpleSwingApplication  {
	def assert[T](t1: => T, t2: T) = if (t1 == t2) () else println("test failed: " + t1.toString + " is not the same as " + t2.toString)
	
	def assert[T](desc: String, t1: => T, t2: T) = if (t1 == t2) () else println(desc +" test failed: " + t1.toString + " is not the same as " + t2.toString)
	
	object Board2By2 extends Solver {
		val width = 2
		val height = 2
		
		assert(allEdges.length, 6)
		assert(allEdges.toString, "List(Edge(Pos(1,1),Pos(1,2)), Edge(Pos(1,1),Pos(2,1)), Edge(Pos(1,1),Pos(2,2)), Edge(Pos(1,2),Pos(2,1)), Edge(Pos(1,2),Pos(2,2)), Edge(Pos(2,1),Pos(2,2)))")
		
		assert(edgesIndices.toString, "Map(Edge(Pos(2,1),Pos(2,2)) -> 5, Edge(Pos(1,1),Pos(2,1)) -> 1, Edge(Pos(1,2),Pos(2,2)) -> 4, Edge(Pos(1,1),Pos(1,2)) -> 0, Edge(Pos(1,2),Pos(2,1)) -> 3, Edge(Pos(1,1),Pos(2,2)) -> 2)")
		
		val s1 = startingBoard extend (Edge(Pos(1, 1), Pos(1, 2)))
		assert(s1.nextEdges.length, 5)
		
		val s2 = startingBoard extend (Edge(Pos(1, 1), Pos(2, 2)))
		assert(s2.nextEdges.length, 4)
		
		val s3 = s2 extend (Edge(Pos(1, 1), Pos(1, 2))) extend (Edge(Pos(1, 1), Pos(2, 1)))
		assert(s3.nextEdges.length, 2)
		
		val s4 = s3 extend (Edge(Pos(2, 1), Pos(2, 2))) extend (Edge(Pos(1, 2), Pos(2, 2)))
		assert(s4.nextEdges.length, 0)
		
		assert(Edge(Pos(0, 0), Pos(2, 2)).intersect(Edge(Pos(2, 0), Pos(0, 2))), true)
		assert(Edge(Pos(0, 0), Pos(2, 2)).intersect(Edge(Pos(2, 2), Pos(3, 10))), false)
		assert(Edge(Pos(0, 0), Pos(2, 2)).intersect(Edge(Pos(0, 0), Pos(0, 2))), false)
		assert(Edge(Pos(0, 0), Pos(2, 2)).intersect(Edge(Pos(0, 0), Pos(2, 0))), false)
		assert(Edge(Pos(0, 0), Pos(2, 2)).contains(Pos(1, 1)), true)
		assert(Edge(Pos(0, 0), Pos(2, 2)).contains(Pos(2, 1)), false)
		assert(Edge(Pos(0, 0), Pos(2, 2)).contains(Pos(0, 0)), false)
		assert(Edge(Pos(0, 0), Pos(2, 2)).contains(Pos(2, 2)), false)
		
		val b4 = startingBoard extend (Edge(Pos(1, 1), Pos(1, 2))) extend (Edge(Pos(1, 1), Pos(2, 2)))
		assert(b4.nextEdges, List(Edge(Pos(1, 2), Pos(2, 2))))
		
		val b5 = startingBoard extend (Edge(Pos(1, 1), Pos(1, 2))) extend (Edge(Pos(1, 1), Pos(2, 2))) extend (Edge(Pos(1, 2), Pos(2, 2))) extend (Edge(Pos(1, 1), Pos(2, 1))) extend (Edge(Pos(2, 1), Pos(2, 2)))
		assert(b5.isCompleted, true)
		
		assert(((for (board <- startingBoard.nextBoards; nextBoard <- board.nextBoards) yield nextBoard) toList) length, 5)
		
		assert(startingBoard.nextBoards.length, 2)
		assert(startingBoard.nextBoards.toString(), "List(Board: List(Edge(Pos(1,1),Pos(1,2))), Board: List(Edge(Pos(1,1),Pos(2,2))))")
		
		val b6 = startingBoard extend (Edge(Pos(1, 2), Pos(2, 2))) extend (Edge(Pos(1, 1), Pos(2, 2)))
		
		val l2Boards = nextBoards(startingBoard.nextBoards)
		assert("here", l2Boards.length, 2)
		assert(l2Boards.toString(), "List(Board: List(Edge(Pos(1,1),Pos(1,2)), Edge(Pos(1,1),Pos(2,2))), Board: List(Edge(Pos(2,1),Pos(2,2)), Edge(Pos(1,1),Pos(1,2))))")
		assert(l2Boards(0).hashEdges, List(0, 2))
		assert(l2Boards(1).hashEdges, List(0, 5))
		
		val l3BoardsForL2_1 = nextBoards(List(l2Boards(1)))
		assert(l3BoardsForL2_1.length, 2)
		assert(l3BoardsForL2_1(0).hashEdges, List(0, 1, 5))
		assert(l3BoardsForL2_1(1).hashEdges, List(0, 2, 5))
		
		assert(trianglesIndices(getTriangle(Pos(2,1), Pos(1,1), Pos(1,2), Player1)), 0)
		assert(trianglesIndices(getTriangle(Pos(2,1), Pos(1,2), Pos(1,1), Player1)), 0)
		assert(trianglesIndices(getTriangle(Pos(2,1), Pos(1,1), Pos(1,2), Player2)), 1)
		assert(trianglesIndices(getTriangle(Pos(1,2), Pos(2,1), Pos(1,1), Player2)), 1)
		assert(trianglesIndices(getTriangle(Pos(1,1), Pos(1,2), Pos(2,1), Player2)), 1)
		
		assert(nextBoards(l2Boards).length, 3)
		
		println(allCompletedBoards.length)
	}
	
	object Board3By3 extends Solver {
		val width = 3
		val height = 3
		
		val e1 = Edge(Pos(1, 1), Pos(1, 2))
		val e2 = Edge(Pos(2, 1), Pos(2, 2))
		val e3 = Edge(Pos(1, 1), Pos(3, 2))
		assert(e1.intersect(e3), false)
		assert(e2.intersect(e3), true)
		
		assert(startingBoard.points.length, 9)
		assert(startingBoard isValidEdge(Edge(Pos(1, 1), Pos(2, 3))), true)
		
		val b1 = startingBoard extend (Edge(Pos(1, 1), Pos(2, 3)))
		assert(b1 isValidEdge (Edge(Pos(1, 3), Pos(2, 1))), false)
		assert(b1 isValidEdge (Edge(Pos(1, 1), Pos(2, 3))), false)
		assert(b1 isValidEdge (Edge(Pos(1, 1), Pos(2, 1))), true)
		assert(b1 isValidEdge (Edge(Pos(1, 1), Pos(2, 2))), true)
		assert(b1 isValidEdge (Edge(Pos(1, 1), Pos(3, 3))), false)
		assert(b1 isValidEdge (Edge(Pos(1, 2), Pos(2, 3))), true)
		
		val b2 = b1 extend (Edge(Pos(1, 2), Pos(2, 3)))
		assert(b2 isValidEdge (Edge(Pos(1, 2), Pos(2, 3))), false)
		assert(b2 isValidEdge (Edge(Pos(1, 2), Pos(1, 1))), true)
		assert(b2 formsTriangle (Edge(Pos(1, 1), Pos(1, 2))), true)
		assert(b2 formsTriangle (Edge(Pos(1, 1), Pos(2, 1))), false)
		
		val b3 = startingBoard extend (Edge(Pos(1, 2), Pos(3, 3))) extend (Edge(Pos(2, 1), Pos(3, 3))) extend (Edge(Pos(1, 2), Pos(2, 1)))
		assert(b3.triangles.head contains Pos(2, 2), true)
		assert(b3.triangles.head contains Pos(2, 1), false)
		assert(b3.triangles.head contains Pos(1, 2), false)
		assert(b3.triangles.head contains Pos(3, 3), false)
		assert(b3.triangles.head contains Pos(1, 1), false)
		assert(b3.triangles.head contains Pos(3, 1), false)
		assert(b3.points.length, 8)

		assert(startingBoard.nextBoards.length, 4)
		assert(startingBoard.nextBoards.toString(), "List(Board: List(Edge(Pos(1,1),Pos(1,2))), Board: List(Edge(Pos(1,1),Pos(2,2))), Board: List(Edge(Pos(1,1),Pos(2,3))), Board: List(Edge(Pos(1,2),Pos(2,1))))")
		
		val b4 = startingBoard extend Edge(Pos(3,2),Pos(3,3)) extend Edge(Pos(2,3),Pos(3,3)) extend Edge(Pos(1,1),Pos(2,1)) extend Edge(Pos(1,1),Pos(3,2)) extend Edge(Pos(1,1),Pos(2,3)) extend Edge(Pos(1,2),Pos(2,3)) extend Edge(Pos(1,3),Pos(2,3)) extend Edge(Pos(3,1),Pos(3,2)) extend Edge(Pos(2,1),Pos(3,2)) extend Edge(Pos(2,3),Pos(3,2)) extend Edge(Pos(2,1),Pos(3,1)) extend Edge(Pos(1,2),Pos(1,3)) extend Edge(Pos(1,1),Pos(1,2))
		assert(b4.nextEdges.length, 0)
		
		val b5 = startingBoard extend Edge(Pos(1,1),Pos(2,2)) extend Edge(Pos(1,2),Pos(2,3))
		val b6 = startingBoard extend Edge(Pos(1,2),Pos(2,3)) extend Edge(Pos(1,1),Pos(2,2))
		val rem = removeSymmetricalBoards(List(b5, b6))
		assert(rem.length, 1)
		
		val l2boards = nextBoards(startingBoard.nextBoards)
		println(l2boards)
		
		//assert(allCompletedBoards.length, 1134)
	}

	
	//Board2By2
	//Board3By3
	
	def top = new MainFrame {
		title = "Hello, World!"
	    contents = Board3By3.ui
	}
}