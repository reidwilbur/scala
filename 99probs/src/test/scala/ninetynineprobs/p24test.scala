package ninetynineprobs

import org.scalatest.FunSuite

class p24test extends FunSuite {
  import p24._

  test("lotto") {
    val nums = lotto(6, 49)
    println(nums)

    assert(nums.length == 6)
    
    assert( nums forall { num => (num >= 1)&&(num <= 49) } )
  }

}

