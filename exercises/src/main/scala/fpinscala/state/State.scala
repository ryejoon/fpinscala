package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (result, newRng) = rng.nextInt
    result match {
      case Int.MinValue => nonNegativeInt(newRng)
      case x if x < 0 => (x * -1, newRng)
      case _ => (result, newRng)
    }
  }

  def nonMinInt(rng: RNG): (Int, RNG) = {
    val (result, newRng) = rng.nextInt
    result match {
      case Int.MinValue => nonMinInt(newRng.nextInt._2)
      case _ => (result, newRng)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    map(nonMinInt)(x => x / Int.MinValue.toDouble)(rng)
  }

  /*def double(rng: RNG): (Double, RNG) = {
    val (result, newRng) = rng.nextInt
    result match {
      case Int.MinValue => double(newRng)
      case _ => (result / Int.MinValue.toDouble, newRng)
    }
  }*/

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (intResult, newRng) = rng.nextInt
    val (doubleResult, newRng2) = double(newRng)
    ((intResult, doubleResult), newRng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, newRng1) = double(rng)
    val (d2, newRng2) = double(newRng1)
    val (d3, newRng3) = double(newRng2)
    ((d1,d2,d3), newRng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def intsInner(c : Int, r : RNG, l : List[Int]): (List[Int], RNG) = c match {
      case x if x <= 0 => (l, r)
      case _ => intsInner(c - 1, r.nextInt._2, r.nextInt._1 :: l)
    }
    intsInner(count, rng, Nil)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    r => {
      val aResult = ra(r)
      val bResult = rb(aResult._2)
      (f(aResult._1, bResult._1), bResult._2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def sequenceInner(remaining: List[Rand[A]], result : List[A], nextRNG : RNG) : (List[A], RNG) =
      remaining match {
        case Nil => (result, nextRNG)
        case item :: l => sequenceInner(l, item(nextRNG)._1 :: result, item(nextRNG)._2)
      }
    rng => sequenceInner(fs, Nil, rng)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}