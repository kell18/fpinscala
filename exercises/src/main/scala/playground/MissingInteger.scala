package playground

object MissingInteger extends App {

  println((0 to 3).toList)
  println(solution((0 to 100000).toArray.map(_ * 10000)))
  println(solution(Array.fill(100)(100)))
  println(solution(Array(1, 3, 6, 4, 1, 2)))
  println(solution(Array(-1, -3)))

  def solution(a: Array[Int]): Int = {
    val positives = a.filter(_ > 0).sorted.distinct
    positives.zipWithIndex
      .find {
        case (num, ind) => num != (ind + 1)
      }
      .fold(ifEmpty = positives.lastOption.getOrElse(0) + 1) { case (_, ind) => ind + 1 }
  }
}
