package ca.vgorcinschi

object KPrimes {

  implicit class StreamOps[T](stream: Stream[T]){

    def takeUntil(test: T => Boolean): Stream[T]={
      stream.span(test) match {
        case (before, after) => before #::: after.take(1)
      }
    }
  }

  def countKprimes(k:  Int, start: Long, end: Long): String = {
    kPrimes(k, start, end).mkString(", ")
  }

  def kPrimes(k: Int, start: Long, end: Long): List[Int] = {
    def loop(n: Int, acc: Stream[Int]): Stream[Int] ={
      if(n > end) acc
      else if(n > 1 && isKPrime(n, k)) loop(n + 1, acc :+ n)
      else loop(n + 1, acc)
    }

    loop(start.toInt, Stream.empty).toList
  }

  private def isKPrime(candidate: Int, kfactors: Int, divisor: Int = 2): Boolean = (candidate, kfactors, divisor) match {
    case (cand, k, _) if cand == 1 => k == 0
    case (cand, _, d) if cand % d == 0 => isKPrime(cand / d, kfactors - 1, d)
    case (_, _, _) => isKPrime(candidate, kfactors, divisor + 1)
  }

  /*
    128, 8 and 1 are respectively the smallest almost primes for 7-k, 3-k, 1-k
   */
  def puzzle(s: Int): Int = {
    val factors = for {
      sevens <- kPrimes(7, 128, s)
      threes <- kPrimes(3, 8, s)
      ones <- kPrimes(1, 1, s)
      if sevens + threes + ones == s
    } yield (sevens, threes, ones)
    factors.size
  }
}
