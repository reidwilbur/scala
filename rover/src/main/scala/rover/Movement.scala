package rover

sealed trait Movement
case object Forward   extends Movement {
  def unapply(c: Char): Boolean = c == 'f'
}

case object Backward  extends Movement {
  def unapply(c: Char): Boolean = c == 'b'
}

case object TurnLeft  extends Movement {
  def unapply(c: Char): Boolean = c == 'l'
}

case object TurnRight extends Movement {
  def unapply(c: Char): Boolean = c == 'r'
}

