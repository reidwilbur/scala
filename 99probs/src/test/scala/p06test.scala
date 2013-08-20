import org.scalatest.FunSuite

package ninetynineprobs {

  class p06test extends FunSuite {
    test("isPalindrome(List(1, 2, 3, 2, 1) == true") {
      assert(true == p06.isPalindrome(List(1, 2, 3, 2, 1)))
    }

    test("isPalindrome(List(1, 1, 3, 2, 1)) == false") {
      assert(false == p06.isPalindrome(List(1, 1, 3, 2, 1)))
    }

    test("isPalindrome(List(1, 2, 2, 1)) == true") {
      assert(true == p06.isPalindrome(List(1, 2, 2, 1)))
    }

    test("isPalindrome(List(1, 1, 2, 1)) == false") {
      assert(false == p06.isPalindrome(List(1, 1, 2, 1)))
    }

    test("isPalindrome(List(1)) == true") {
      assert(true == p06.isPalindrome(List(1)))
    }
  }

}

