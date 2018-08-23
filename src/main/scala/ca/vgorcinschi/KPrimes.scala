package ca.vgorcinschi

object KPrimes {
  def countKprimes(k:  Int, start: Long, end: Long): String = {
    countHelper(k, start, end).mkString(", ")
  }

  def countHelper(k: Int, start: Long, end: Long): List[Int] = {
    def loop(n: Int): Stream[Int] =
      if(isKPrime(n, k)) n #:: loop(n + 1)
      else loop(n + 1)
    loop(2).dropWhile(_.toLong < start).takeWhile(_.toLong < end).toList
  }

  private def isKPrime(candidate: Int, kfactors: Int, divisor: Int = 2): Boolean = (candidate, kfactors, divisor) match {
    case (cand, k, _) if cand == 1 => k == 0
    case (cand, _, d) if cand % d == 0 => isKPrime(cand / d, kfactors - 1, d)
    case (_, _, _) => isKPrime(candidate, kfactors, divisor + 1)
  }

  def puzzle(s: Int): Int = {
    ???
  }
}
