package triangula

sealed abstract class Player

case object Player1 extends Player {
	override def toString() = "x"
}

case object Player2 extends Player {
	override def toString() = "o"
}

case object PlayerUnknown extends Player {
	override def toString() = "?"
}