package triangula

sealed abstract class Player {
  def next(): Player;
}

case object Player1 extends Player {
  override def next() = Player2
  override def toString() = "x"
}

case object Player2 extends Player {
  override def next() = Player1
  override def toString() = "o"
}

case object PlayerUndefined extends Player {
  override def next() = throw new Exception()
  override def toString() = "?"
}