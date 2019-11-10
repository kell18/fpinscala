package playground

import scala.collection.{mutable => m}

class FrogRiverOne

object Solution extends App {

  // println(solution(4, Array(10, 9, 8, 1, 2, 3, 4, 5)))
  println(solution(100000, (1 to 100000).toSet.toArray.map(_ => 300)))

  def solution(x: Int, a: Array[Int]): Int = if (x <= a.length && x > 0) {
    val spotsSet = m.Set(1 to x :_*)
    a.zipWithIndex
      .find { case (spot, _) =>
        spotsSet -= spot
        spotsSet.isEmpty
      }
      .fold(ifEmpty = -1) { case (_, time) => time }
  } else {
    -1
  }
}

