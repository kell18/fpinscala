package playground

object PassingCars extends App {

  val t1 = (1 to 20001).map(_ => 0) ++ (1 to 50000).map(_ => 1)

  println(solution(t1.toArray))
  println(solution(Array(0, 1, 0, 1, 1)))
  println(solution(Array(1, 1, 1, 1, 1, 1, 1)))
  println(solution(Array(0, 0, 0, 0, 0, 0, 0)))

  def solution(a: Array[Int]): Int = {
    val (_, totalPassed) = a.foldRight(0 -> 0) {
      // if direction=0 then traveling east (increase total passed cars)
      case (0, (wests, passed)) => wests -> (passed + wests)
      // if direction=1 then traveling west (increase total cars traveling west
      case (1, (wests, passed)) => (wests + 1) -> passed

      case (d, _) => sys.error(s"Wrong direction: $d")
    }
    if (totalPassed > 1000000000) -1 else totalPassed
  }
}
