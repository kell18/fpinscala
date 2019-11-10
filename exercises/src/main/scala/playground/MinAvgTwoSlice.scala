package playground

object MinAvgTwoSlice extends App {
  println(solution(Array(1, 2, 1, 1, 1)))
  println(solution(Array.fill(100000)(-10000)))
  println(solution(Array.fill(100000)(10000)))
  println(solution((-10000 to 10000).toArray))
  println(solution((-10000 to 10000).reverse.toArray))

  // .. still need 3-sized slices (and it's not clear why)
  def solution(a: Array[Int]): Int = {
    var minAvg = Int.MaxValue.toDouble
    var minInd = 0
    for (i <- 1 until a.length) {
      val nextAvg = (a(i-1) + a(i)) / 2.0
      if (nextAvg < minAvg) {
        minAvg = nextAvg
        minInd = i - 1
      }
    }
    minInd
  }
}
