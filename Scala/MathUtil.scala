import scala.collection.mutable.BitSet

object MathUtil {
  def primeUnder(n: Int): IndexedSeq[Int] = {
    val isPrime = new BitSet(n)

    isPrime += 2
    for (i <- 3 until n by 2) isPrime += i
    for (p <- 3 until n by 2) {
      if (isPrime(p)) {

        val pLong = p.toLong
        for (s <- pLong*pLong until n by pLong) {
          isPrime -= s.toInt
        }
      }
    }

    isPrime.toIndexedSeq
  }

  def main(args: Array[String]): Unit = {
    println(primeUnder(100).mkString(" "))
  }
}
