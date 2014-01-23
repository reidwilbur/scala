package rover

import scala.annotation.tailrec

case class Planet(size: Size = InfiniteSize, obstacles: List[Coordinate] = List())

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

