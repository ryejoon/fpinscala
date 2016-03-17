package fpinscala.state

import org.scalatest.FunSuite

/**
  * Created by ryejoon on 3/17/16.
  */
class RNG$Test extends FunSuite {

  test("nonNegativeInt should generate random numbers") {
    assert(RNG.nonNegativeInt(RNG.Simple(100))._1 >= 0)
  }

}
