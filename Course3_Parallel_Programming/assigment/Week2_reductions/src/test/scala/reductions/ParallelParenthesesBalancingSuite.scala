package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)

    assert(parBalance("".toArray, 1))
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) === expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)

    def checkPar(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 2) === expected,
        s"balance($input) should be $expected")

    checkPar("(", false)
    checkPar(")", false)
    checkPar(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) === expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)

    def checkPar(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 2) === expected,
        s"balance $input should be $expected")

    checkPar("()", true)
    checkPar(")(", false)
    checkPar("((", false)
    checkPar("))", false)
    checkPar(".)", false)
    checkPar(".(", false)
    checkPar("(.", false)
    checkPar(").", false)

  }
  test("parbalance for some longer array") {
    assert(parBalance("(()()(()))".toArray, 3))
    assert(parBalance("(()()()((())))".toArray, 3))
    assert(!parBalance("())))))(((((()".toArray, 3))
    assert(!parBalance("(()()()())))".toArray, 3))

    assert(parBalance("(  (_((_) ()()  (_))((_)  ()()  (_))_) )".toArray, 3))
    assert(!parBalance("(  (_((_) ()(_)_)  (_))((_)  ()()  (_))_) )".toArray, 3))

  }

}