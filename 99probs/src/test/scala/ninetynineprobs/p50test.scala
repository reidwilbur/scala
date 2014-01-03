package ninetynineprobs

import org.scalatest.FunSuite

class p50test extends FunSuite {
  import S99Logic._

  test("huffmanMut(List(('a, 45), ('b, 13), ('c, 12), ('d, 16), ('e, 9), ('f, 5))) == List(('a,0), ('b,101), ('c,100), ('d,111), ('e,1101), ('f,1100))") {
    var codes = huffmanMut(List(('a, 45), ('b, 13), ('c, 12), ('d, 16), ('e, 9), ('f, 5)))

    var exp = List(('a, "0"), ('b,"101"), ('c,"100"), ('d,"111"), ('e,"1101"), ('f,"1100"))

    println("Got:")
    println(codes)
    println("Exp:")
    println(exp)

    assert(codes.length == exp.length)
    exp.foreach { code => assert( codes.contains(code) ) }
  }

  test("huffmanImm(List(('a, 45), ('b, 13), ('c, 12), ('d, 16), ('e, 9), ('f, 5))) == List(('a,0), ('b,101), ('c,100), ('d,111), ('e,1101), ('f,1100))") {
    var codes = huffmanImm(List(('a, 45), ('b, 13), ('c, 12), ('d, 16), ('e, 9), ('f, 5)))

    var exp = List(('a, "0"), ('b,"101"), ('c,"100"), ('d,"111"), ('e,"1101"), ('f,"1100"))

    println("Got:")
    println(codes)
    println("Exp:")
    println(exp)

    assert(codes.length == exp.length)
    exp.foreach { code => assert( codes.contains(code) ) }
  }

}

