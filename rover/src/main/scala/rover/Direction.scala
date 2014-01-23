package rover

sealed trait Direction {
  def opposite: Direction
  def left: Direction
  def right: Direction
  def move(start: Coordinate, gridSize: Size): Coordinate
}

case object North extends Direction {
  override def opposite: Direction = South
  override def left: Direction     = West
  override def right: Direction    = East
  override def move(start: Coordinate, gridSize: Size) = {
    Coordinate(gridSize.wrapX(start.x), gridSize.wrapY(start.y+1))
  }
}

case object South extends Direction {
  override def opposite: Direction = North
  override def left: Direction     = East
  override def right: Direction    = West
  override def move(start: Coordinate, gridSize: Size) = {
    Coordinate(gridSize.wrapX(start.x), gridSize.wrapY(start.y-1))
  }
}

case object East extends Direction {
  override def opposite: Direction = West
  override def left: Direction     = North
  override def right: Direction    = South
  override def move(start: Coordinate, gridSize: Size) = {
    Coordinate(gridSize.wrapX(start.x+1), gridSize.wrapY(start.y))
  }
}

case object West extends Direction {
  override def opposite: Direction = East
  override def left: Direction     = South
  override def right: Direction    = North
  override def move(start: Coordinate, gridSize: Size) = {
    Coordinate(gridSize.wrapX(start.x-1), gridSize.wrapY(start.y))
  }
}

