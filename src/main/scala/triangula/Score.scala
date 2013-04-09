package triangula

/**
 * Contains scores/statistics for a given board. This object should be created after
 * the computation is over, otherwise you'll get zeros as your scores.
 */
class Score(board: Board) {
  val player1Area = getArea(board, Player1)
  val player2Area = getArea(board, Player2)

  val scores: Map[Player, Int] = (board.isCompleted, player1Area.compare(player2Area)) match {
    case (true, 1)  => add(Player1)
    case (true, -1) => add(Player2)
    case (true, 0)  => add(PlayerUndefined)
    case _ => {
      def merge(map1: Map[Player, Int], map2: Map[Player, Int]): Map[Player, Int] =
        map1 ++ map2.map { case (k, v) => k -> (v + map1.getOrElse(k, 0)) }
      board.children.map(t => t.score.scores).fold(Score.emptyMap())((res, item) => merge(res, item))
    }
  }

  def add(p: Player): Map[Player, Int] = Score.emptyMap updated (p, 1)

  /**
   * Gets area occupied by a given player.
   */
  def getArea(board: Board, p: Player) =
    if (board.triangles.isEmpty) 0
    else math.ceil(100 * board.triangles.filter(t => t.player == p).map(t => t.area).sum / ((board.definition.dim.width - 1) * (board.definition.dim.height - 1)))

  lazy val total = scores.map(_._2).sum toFloat
  lazy val player1Score = scores(Player1)
  lazy val player2Score = scores(Player2)
  lazy val drawScore = scores(PlayerUndefined)
}

object Score {
  def emptyMap(): Map[Player, Int] = List((Player1, 0), (Player2, 0), (PlayerUndefined, 0)) toMap

  def apply(board: Board): Score = new Score(board)
}