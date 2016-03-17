import scala.annotation.tailrec

object E2_2 {
  def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
    if (as.isEmpty) return true
    @tailrec
    def loop(index : Int) : Boolean = {
      if (as.size - 1 == index) return true
      if (ordered(as(index), as(index + 1))) return loop(index + 1)
      else return false
    }
    loop(0)
  }
}

val intOrderComparator = (a1: Int, a2: Int) => (a1 <= a2)
E2_2.isSorted(Array(), intOrderComparator)
E2_2.isSorted(Array(1), intOrderComparator)
E2_2.isSorted(1 to 10 toArray, intOrderComparator)
E2_2.isSorted(10 to 1 by -1 toArray, intOrderComparator)


object E2_3 {
  def curry[A,B,C] (f: (A,B) => C) : A => (B => C) = {
    (a : A) => (b : B) => f(a, b)
  }

  def curry2[A,B,C] (f: (A,B) => C) : A => (B => C) = {
    (a : A) => f(a, _)
  }
}

object E_2_5 {
  def compose[A, B, C] (f: B => C, g: A => B) : A => C = {
    (a : A) => f(g(a))
  }
}


