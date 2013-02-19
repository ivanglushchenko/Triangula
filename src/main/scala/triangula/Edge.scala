package triangula

/**
 * Represents a single edge on a board.
 */
case class Edge(from: Pos, to: Pos){
	def intersect(e: Edge): Boolean = 
		if ((from == e.from && to == e.to) || (from == e.to && to == e.from)) true
		else if ((from == e.from && to != e.to) || (from != e.from && to == e.to) || (from == e.to && to != e.from) || (from != e.to && to == e.from)) false
		else {
			if (to.x != from.x) {
				val t2 = ((e.from.y - from.y) * (to.x - from.x) - (e.from.x - from.x) * (to.y - from.y)).toFloat / ((e.to.x - e.from.x) * (to.y - from.y) - (e.to.y - e.from.y) * (to.x - from.x)).toFloat
				val t1 = ((e.to.x - e.from.x) * t2 + e.from.x - from.x).toFloat / (to.x - from.x).toFloat
				t2 >=0 && t2 <= 1 && t1 >= 0 && t1 <= 1
			}
			else {
				val t2 = ((e.from.x - from.x) * (to.y - from.y) - (e.from.y - from.y) * (to.x - from.x)).toFloat / ((e.to.y - e.from.y) * (to.x - from.x) - (e.to.x - e.from.x) * (to.y - from.y)).toFloat
				val t1 = ((e.to.y - e.from.y) * t2 + e.from.y - from.y).toFloat / (to.y - from.y).toFloat
				t2 >=0 && t2 <= 1 && t1 >= 0 && t1 <= 1
			}
		}
	
	def contains(p: Pos): Boolean =
		if (p == from || p == to) false
		else {
			if (to.x != from.x) {
				val t = (p.x - from.x).toFloat / (to.x - from.x).toFloat
				if (t > 1 || t < 0) false
				else {
					val y = (to.y - from.y) * t + from.y
					math.abs(y - p.y) < 0.00001
				}
			} else {
				val t = (p.y - from.y).toFloat / (to.y - from.y).toFloat
				if (t > 1 || t < 0) false
				else {
					val x = (to.x - from.x) * t + from.x
					if (math.abs(x - p.x) < 0.00001) true
					else false
				}
			}
		}
	
	def isSame(e: Edge) = (from == e.from && to == e.to) || (from == e.to && to == e.from)
}
