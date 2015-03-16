package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange

  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  test("countChange: my1") {
    assert(countChange(12,List(3,4)) === 2)
  }

  test("countChange: my2") {
    assert(countChange(12,List(3,6)) === 3)
  }

  test("countChange: my3") {
    assert(countChange(12,List(4,6)) === 2)
  }

  test("countChange: my4") {
    assert(countChange(10,List(1,2)) === 6)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }
}
