package playground

object PermCheck extends App {

  // println(solution((1 to 100000).toArray))
  // println(solution(Array(7, 1, 1, 1)))
  println(solution(Array(4, 1, 3, 2)))

  def solution(a: Array[Int]): Int = {
    if (a.length == 1 && a.head == 1) 1
    else if (isSeqLinear_mut(a)) 1
    else 0
  }

  def isSeqLinear_mut(a: Array[Int]): Boolean = {
    a.indices
      .foreach { ind =>
        val num = a(ind)
        if (num <= a.length) {
          a(ind) = a(num - 1)
          a(num - 1) = num
        }
      }

    a
      .zipWithIndex
      .find { case (num, ind) => num - 1 != ind }
      .fold(ifEmpty = true)(_ => false)
  }
}

/*
3 1 4 5 2
4 1 3
1 4 3
1 5 3 4
1 2 3 4 5
*/
