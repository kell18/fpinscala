package playground

object Counters extends App {

  // println(solution(5, Array(3, 4, 4, 6, 1, 4, 4)).mkString(", "))
  // println(solution(100000, Array(3, 4, 4, 6, 1, 4, 4, 4, 4, 4, 100001)).mkString(", "))

  val testA = Array.fill(100001)(4)
  testA(100000) = 100001

  println(solution(100000, testA).mkString(", "))

  def solution(n: Int, operations: Array[Int]): Array[Int] = {
    val counters = Array.fill(n)(0)
    val (_, lastMaximization) = operations.foldLeft(0 -> 0) {
      // in the special case when we need to do the maximization of all the elems we just set
      // prevMaximization to maxCounter (and handle it later on)
      case ((maxCounter, _), op) if op == n+1 => maxCounter -> maxCounter

      case ((maxCounter, prevMaximization), op) if op <= n =>
        val prevCnt = counters(op-1)
        val currCnt = Math.max(prevCnt, prevMaximization) + 1
        counters(op-1) = currCnt
        Math.max(maxCounter, currCnt) -> prevMaximization

      case (_, op) => sys.error(s"Invalid operation: $op, should be <= n+1 (${n+1})")
    }
    counters.map(c => Math.max(c, lastMaximization))
  }
}
