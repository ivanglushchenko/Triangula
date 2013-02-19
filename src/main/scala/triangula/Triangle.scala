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
	
	def map(mappings: Map[Pos, Pos]): Triangle =
		Triangle.getCanonicalTriangle(mappings(p1), mappings(p2), mappings(p3), player)
}

object Triangle {
	/**
	 * Gets a "canonical" representation of a triangle. The idea: given 3 points A,B,C you
	 * can define a triangle in 6 different ways: ABC, ACB,  CBA, and so on. This function
	 * returns triangle ABC for any of the combinations of points.  
	 */
	def getCanonicalTriangle(p1: Pos, p2: Pos, p3:Pos, p: Player): Triangle = {
		val s1 = 
			if (p1 < p2 && p1 < p3) p1
			else if (p2 < p1 && p2 < p3) p2
			else p3
		val s3 = 
			if (p1 > p2 && p1 > p3) p1
			else if (p2 > p1 && p2 > p3) p2
			else p3		
		val s2 = 
			if (p1 != s1 && p1 != s3) p1
			else if (p2 != s1 && p2 != s3) p2
			else p3
		if (s1 == s2 || s1 == s3 || s2 == s3) throw new Exception("bad triangle")
		new Triangle(s1, s2, s3, p)
	}
}