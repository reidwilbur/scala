package rover

import scala.annotation.tailrec

case class Coordinate(x: Int, y: Int)

sealed trait Size {
  def x: Int
  def y: Int
  def wrapX(i: Int): Int
  def wrapY(i: Int): Int
}
case class FiniteSize(x: Int, y: Int) extends Size {
  def wrap(i: Int, max: Int): Int = {
    val m = i % max
    if (m < 0) max + m else m
  }
  override def wrapX(i: Int): Int = wrap(i, x)
  override def wrapY(i: Int): Int = wrap(i, y)
}
case object InfiniteSize extends Size {
  override def x = -1
  override def y = -1
  override def wrapX(i: Int) = i
  override def wrapY(i: Int) = i
}

case class Planet(size: Size = InfiniteSize, obstacles: List[Coordinate] = List())

object RoverImplicits {
  implicit def tupleToCoord(tuple: Tuple2[Int,Int]): Coordinate = {
    Coordinate(tuple._1, tuple._2)
  }
  
  implicit def tupleToSize(tuple: Tuple2[Int,Int]): Size ={
    FiniteSize(tuple._1, tuple._2)
  }
}

sealed trait Direction {
  def opposite: Direction
  def left: Direction
  def right: Direction
  def move(start: Coordinate, gridSize: Size): Coordinate
}
case object North extends Direction {
  override def opposite: Direction = South
  override def left: Direction = West
  override def right: Direction = East
  override def move(start: Coordinate, gridSize: Size) = {
    Coordinate(gridSize.wrapX(start.x),   gridSize.wrapY(start.y+1))
  }
}
case object South extends Direction {
  override def opposite: Direction = North
  override def left: Direction = East
  override def right: Direction = West
  override def move(start: Coordinate, gridSize: Size) = {
    Coordinate(gridSize.wrapX(start.x),   gridSize.wrapY(start.y-1))
  }
}
case object East  extends Direction {
  override def opposite: Direction = West
  override def left: Direction = North
  override def right: Direction = South
  override def move(start: Coordinate, gridSize: Size) = {
    Coordinate(gridSize.wrapX(start.x+1),   gridSize.wrapY(start.y))
  }
}
case object West  extends Direction {
  override def opposite: Direction = East
  override def left: Direction = South
  override def right: Direction = North
  override def move(start: Coordinate, gridSize: Size) = {
    Coordinate(gridSize.wrapX(start.x-1),   gridSize.wrapY(start.y))
  }
}

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

object Rover {
  def apply(coord: Coordinate, dir: Direction, planet: Planet = Planet()) = {
    new Rover(coord, dir, planet)
  }
}

class Rover(val startCoord: Coordinate, val startDir: Direction, val planet: Planet) {
  var coordinates = startCoord
  var direction = startDir
  
  def sendCommands(cmds: String): (String,List[Char]) = {
    @tailrec
    def processCmds(unProcd: List[Char], procd: List[Char]): List[Char] = {
      if (unProcd == Nil) 
        procd
      else {
        val (nextCoord, nextDir) = 
          unProcd.head match {
            case Forward() => 
              (direction.move(coordinates, planet.size), direction)
            case Backward() =>
              (direction.opposite.move(coordinates, planet.size), direction)
            case TurnLeft() =>
              (coordinates, direction.left)
            case TurnRight() =>
              (coordinates, direction.right)
          }
      
        if (planet.obstacles.contains(nextCoord))
          procd
        else {
          coordinates = nextCoord
          direction = nextDir
          processCmds(unProcd.tail, unProcd.head :: procd)
        }
      }
    }
    val procd = processCmds(cmds.toList, List()).reverse
    if (procd.size == cmds.size) ("OK",procd) else ("NOK",procd)
  }
}