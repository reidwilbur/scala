package ch3

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BinPackSuite extends FunSuite {
  import BinPack._
  
  test("best fit, single weight") {
    assert(BinPack.packBestFit(List(0.5)) == 1)
  }

  test("best fit, 2 weights 1 bin") {
    assert(BinPack.packBestFit(List(0.5, 0.2)) == 1)
  }

  test("best fit, 2 weights, 1 bin") {
    assert(BinPack.packBestFit(List(0.2, 0.5)) == 1)
  }
  
  test("best fit, 3 weights, 2 bins") {
    assert(BinPack.packBestFit(List(0.2, 0.5, 0.4)) == 2)    
  }
  
  test("best fit, 4 weights, 2 bins") {
    assert(BinPack.packBestFit(List(0.2, 0.5, 0.4, 0.4)) == 2)    
  }
  
  test("best fit, 5 weights, 2 bins") {
    assert(BinPack.packBestFit(List(0.2, 0.5, 0.4, 0.4, 0.3)) == 2)    
  }
  
  test("worst fit, single weight") {
    assert(BinPack.packWorstFit(List(0.5)) == 1)    
  }
  
  test("worst fit, 4 weights fit in 2 bins, worst fit takes 3") {
    assert(BinPack.packWorstFit(List(0.2, 0.9, 0.09, 0.8)) == 3)
  }
  
  test("best fit, 4 weights fit in 2 bins, best fit takes 2") {
    assert(BinPack.packBestFit(List(0.2, 0.9, 0.09, 0.8)) == 2)
  }
}