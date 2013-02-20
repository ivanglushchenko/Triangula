package triangula

/**
 * Represents a single point on a board.
 */
case class Pos(x: Int, y: Int) {
  def -(t: Pos): (Int, Int) = (t.x - x, t.y - y)
  def <(t: Pos): Boolean = x < t.x || (x == t.x && y < t.y)
  def >(t: Pos): Boolean = x > t.x || (x == t.x && y > t.y)
}