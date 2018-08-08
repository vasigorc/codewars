package ca.vgorcinschi

import ca.vgorcinschi.WalkerTest._
import org.scalatest.Assertions._
import org.scalatest._

class WalkerTest extends FlatSpec {
  it should "pass basic tests" in {
    dotest(12, 20, 18, 45, 30, 60, Array(15, 135, 49, 18))
    dotest(15,15,19,50,29,55, Array(12, 133, 18, 44))
    dotest(14,25,17,41,35,59, Array(20, 129, 41, 57))

  }
}

object WalkerTest {
  private def dotest(a: Int, b: Int, c: Int, aa: Int, bb: Int, cc: Int, expect: Array[Int]): Unit = {
    val actual: Array[Int] = Walker.solve(a, b, c, aa, bb, cc)
    assertResult(expect){actual}
  }
}