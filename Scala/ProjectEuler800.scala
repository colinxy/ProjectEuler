import scala.math.{log,Ordering};
import scala.collection.Searching.{Found,InsertionPoint};

object ProjectEuler800 {
  def cLog(nLog: Double): Int = {
    val primes = MathUtil.primeUnder((nLog*1.5).toInt)

    0.until(primes.length).map { i =>
      findUpper(primes, i, nLog) match {
        case Some(j) => j-i
        case None => 0
      }
    }.sum
  }

  /**
    * Find max q of hybrid-integer.
    *
    * Given p index, returns q index.
    *
    * Hacks scala's binary search to find the first q
    * such that the resulting hybrid-integer is greater than or equal to the limit
    */
  def findUpper(primes: IndexedSeq[AnyVal], pIdx: Int, nLog: Double): Option[Int] = {
    val p = primes(pIdx).asInstanceOf[Int]

    val result = primes.search(nLog, pIdx+1, primes.length)(new Ordering[AnyVal] {
      def compare(nLogAnyVal: AnyVal, qAnyVal: AnyVal): Int = {
        val nLog = nLogAnyVal.asInstanceOf[Double]
        val q = qAnyVal.asInstanceOf[Int]

        val h = q*log(p) + p*log(q)
        Ordering[Double].compare(nLog, h)
      }
    })

    result match {
      case Found(qIdx) => Some(qIdx)
      case InsertionPoint(qIdx) => {
        if qIdx > pIdx+1 then Some(qIdx-1)
        else None
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(cLog(800*log(800)))
    println(cLog(800800*log(800800)))
  }
}
