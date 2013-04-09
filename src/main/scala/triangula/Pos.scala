package triangula

/**
 * Represents a single point on a board.
 */
case class Pos(x: Int, y: Int) extends Ordered[Pos] {
  def compare(that: Pos): Int = 
    if (x == that.x) y.compare(that.y) else x.compare(that.x)
  
  def -(t: Pos): (Int, Int) = (t.x - x, t.y - y)
}