package fpinscala.state

import org.scalatest.FunSuite

/**
  * Created by ryejoon on 3/17/16.
  */
class RNG$Test extends FunSuite {

  test("nonNegativeInt should generate random numbers") {
    assert(RNG.nonNegativeInt(RNG.Simple(100))._1 >= 0)
  }

  test("double should generate random doubles") {
    println(RNG.double(RNG.Simple(100)))
    println(RNG.double3(RNG.Simple(100)))
    println(RNG.doubleInt(RNG.Simple(100)))
    println(RNG.intDouble(RNG.Simple(100)))
    //assert()._1 >= 0)
  }

}
