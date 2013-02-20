package triangula

import scala.collection.immutable._

/**
 * Contains logic for solving the puzzle.
 */
trait Solver extends SymmetryMapper {
  lazy val startingBoard: Board = Board(this)

  /**
   * Gets boards generated from the given list of boards.
   */
  def nextBoards(boards: List[Board]): List[Board] =
    removeSymmetricalBoards(boards.flatMap(b => removeSymmetricalBoards(b.nextBoards)))

  /**
   * Generates a stream of boards.
   */
  def next(boards: List[Board]): Stream[Board] = boards match {
    case Nil => Stream.Empty
    case _ => {
      println("..generating " + (boards.head.edges.length + 1).toString + "-edge boards")
      val list = nextBoards(boards)
      println("..done, there are " + list.length.toString + " of them")

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
    startingBoard.score
    println("..done")
    boards
  }
}