package triangula

/**
 * Represents a single triangle on a board.
 */
case class Triangle(p1: Pos, p2: Pos, p3: Pos, player: Player){
	val area = {
		def length(p1: Pos, p2: Pos): Double = math.sqrt((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y))
		val l1 = length(p1, p2)
		val l2 = length(p2, p3)
		val l3 = length(p1, p3)
		val s = (l1 + l2 + l3) / 2.0
		math.sqrt(s * (s - l1) * (s - l2) * (s - l3))
	}
	
	def dot(v1: (Int, Int), v2: (Int, Int)): Double = v1._1 * v2._1 + v1._2 * v2._2
	
	def contains(p: Pos): Boolean = {
		val v0 = p2 - p1
		val v1 = p3 - p1
		val v2 = p  - p1
		val dot00 = dot(v0, v0)
		val dot01 = dot(v0, v1)
		val dot02 = dot(v0, v2)
		val dot11 = dot(v1, v1)
		val dot12 = dot(v1, v2)			
		val invDenom = 1 / (dot00 * dot11 - dot01 * dot01)
		val u = (dot11 * dot02 - dot01 * dot12) * invDenom
		val v = (dot00 * dot12 - dot01 * dot02) * invDenom
		(u > 0) && (v > 0) && (u + v < 1)
	}
}