package rover

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class RoverTest extends FlatSpec with Matchers {
  import rover.RoverImplicits._
 
  "Planet" can "define its size" in {
    val mars = Planet((5, 6))
    mars.size.x should be (5)
    mars.size.y should be (6)
  }
 
  "Rover" should "accept starting point (X,Y) of a rover and the direction (N,S,E,W) it is facing" in {
    val rover = Rover((12, 42), East)
    rover.coordinates.x should be (12)
    rover.coordinates.y should be (42)
    rover.direction should be (East)
  }
 
  it should "be able to move forward (f)" in {
    val rover = Rover((12, 42), East)
    rover.sendCommands("f")
    rover.coordinates.x should be (13)
    rover.coordinates.y should be (42)
  }
 
  it should "be able to move backward (b)" in {
    val rover = Rover((12, 42), East)
    rover.sendCommands("b")
    rover.coordinates.x should be (11)
    rover.coordinates.y should be (42)
  }
 
  it should "be able to turn left (l)" in {
    val rover = Rover((12, 42), North)
    rover.sendCommands("l")
    rover.direction should be (West)
  }
 
  it should "be able to receive a character array of commands" in {
    val rover = Rover((12, 42), East)
    rover.sendCommands("flf")
    rover.coordinates.x should be (13)
    rover.coordinates.y should be (43)
    rover.direction should be (North)
  }
 
  it should "wrap from one edge of the grid to another" in {
    val rover = Rover((1, 1), East, Planet((3, 3)))
    rover.sendCommands("fff")
    rover.coordinates.x should be (1)
    rover.sendCommands("rfff")
    rover.coordinates.y should be (1)
  }
 
  it should "report OK and array of commands if no obstacle was found" in {
    val rover = Rover((12, 42), East)
    rover.sendCommands("f") should be (("OK",List('f')))
  }
 
  it should "report NOK and array of commands that lead to an obstacle" in {
    val rover = Rover((1, 1), North, Planet((10, 10), List((1, 3))))
    rover.sendCommands("ff") should be (("NOK",List('f')))
  }
 
}
