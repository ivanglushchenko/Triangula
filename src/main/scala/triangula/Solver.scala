package triangula

/**
 * Contains logic for solving the puzzle. 
 */
trait Solver extends GameDef {
	lazy val startingBoard: Board = Board.Board()
	
	/**
	 * Gets boards generated from the given list of boards.
	 */
	def nextBoards(boards: List[Board]): List[Board] = {
		val list = (for {
			board <- boards
			nextBoard <- removeSymmetricalBoards(board.nextBoards)
		} yield nextBoard) toList

		removeSymmetricalBoards(list)
	}
	
	/**
	 * Prunes symmetrical boards.
	 */
	def removeSymmetricalBoards(list: List[Board]): List[Board] = {
		def isSmaller(l1: List[Int], l2: List[Int]): Boolean = {
			if (l1.isEmpty) false
			else {
				if (l1.head == l2.head) isSmaller(l1.tail, l2.tail)
				else l1.head < l2.head 
			}
		}
		
		def getLowerSymmHash(h1: List[Int], h2: List[Int], m1: scala.collection.immutable.Map[Int, Int], m2: scala.collection.immutable.Map[Int, Int]): (List[Int], List[Int]) = {
			val h1s = h1 map(t => m1(t)) sortBy(t => t)
			val h2s = h2 map(t => m2(t)) sortBy(t => t)
			val r1 = if (isSmaller(h1, h1s)) h1 else h1s
			val r2 = if (isSmaller(h2, h2s)) h2 else h2s
			(r1, r2)
		}
		
		def compactify(l: List[Board], symMap1: scala.collection.immutable.Map[Int, Int], symMap2: scala.collection.immutable.Map[Int, Int]): List[Board] = {
			val sets = l groupBy(t => getLowerSymmHash(t.hashEdges, t.hashTriangles, symMap1, symMap2)) map(t => t._2.sortWith((b1, b2) => isSmaller(b1.hashEdges, b2.hashEdges))) toList

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

		def applyNextSymmetry(boards: List[Board], symm: List[(scala.collection.immutable.Map[Int, Int], scala.collection.immutable.Map[Int, Int])]): List[Board] =
			if (symm.isEmpty) boards
			else applyNextSymmetry(compactify(boards, symm.head._1, symm.head._2), symm.tail)
			
		applyNextSymmetry(list, symmetries)
	}
	
	/**
	 * Generates a stream of boards.
	 */
	def next(boards: List[Board]): Stream[Board] = {
		if (boards.isEmpty) Stream.Empty
		else {
			val l = boards.head.edges.length + 1
			println("..generating " + l.toString + "-edge boards")

			val list = nextBoards(boards)

			var c = list.length
			println("..done, there are " + c.toString + " of them")
			if (list.isEmpty) Stream.Empty
			else Stream.concat(list toStream, next(list))
		}
	}
	
	/**
	 * A stream of all possible boards.
	 */
	lazy val allBoardsStream: Stream[Board] = next(startingBoard.nextBoards)
	
	/**
	 * A stream of all completed boards.
	 */
	lazy val allCompletedBoardsStream = allBoardsStream filter (_.isCompleted)
	
	lazy val allCompletedBoards = {
		val boards = allCompletedBoardsStream.take(10000).toList
		println("..calc scores")
		startingBoard.scores
		println("..done")
		boards
	}
}