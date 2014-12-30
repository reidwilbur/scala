package fpinscala.parallelism

import java.util.concurrent.Executors

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParSuite extends FunSuite {

  val pool = Executors.newFixedThreadPool(4);

  test("parFilter filters elements")
  {
    val ints = List[Int](1,5,2,6,3,7,4,8);

    val filterFuture = Par.parFilter(ints)(_ >= 5)(pool)

    val filteredInts = filterFuture.get()

    assert(filteredInts == List[Int](5,6,7,8))
  }
}
