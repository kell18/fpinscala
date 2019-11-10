/*

import java.io._

object MinSwaps2 extends App {

  println(minimumSwaps2(List(4, 3, 1, 2).toArray))

  def minimumSwaps2(arr: Array[Int]): Int = {
    var swaps = 0
    val index = ??? // Array.fill(arr.length)
    arr.zipWithIndex.foreach { case (id, ind) =>
      index(id-1) = ind
    }

    def isSwapNeeded(num: Int, id: Int) = num < id
    def wasAnySwap(num: Int, id: Int) = {

      num > id
    }

    for (num <- 1 to arr.length) {

      val swapI = index(num-1)
      println(swapI)

      val id = arr(num-1)
      val el = if (wasAnySwap(num, id)) {
        arr(id - 1)
      } else {
        id
      }

      if (isSwapNeeded(num, el)) {
        println(s"swap ${(num, arr(num-1), arr(swapI), el)}")
        swaps += 1
        val t = arr(swapI)
        arr(swapI) = t
        arr(num-1) = num
        val t1 = index(el - 1)
        index(el - 1) = t
      } else {
        println(s"skip ${(num, arr(num-1), arr(swapI), el)}")
      }
    }
    swaps
  }


  def minimumSwaps1(arr: Array[Int]): Int = {
    var swaps = 0
    for (num <- 1 to arr.length) {
      val el = arr(num-1)
      if (el == num || (num < arr.length && el == num+1 && arr(num) == num)) {}
      else {
        swaps += 1
      }
    }

    Math.max(0, swaps-1)
  }

  // Complete the minimumSwaps function below.
  def minimumSwaps(elemsWithIdxs: List[(Int, Int)]): Int = {
    elemsWithIdxs.foldLeft(0) {
      case (swaps, (el, ind)) if ind == elemsWithIdxs.length - 1 =>
        if (el == ind + 1 && swaps > 0) swaps - 1 else swaps

      case (swaps, (el, ind)) =>
        if (el == ind + 1) swaps else swaps + 1
    }
  }



  def main1(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val n = stdin.readLine.trim.toInt

    val arr = stdin.readLine.split(" ").map(_.trim.toInt)
    val res = minimumSwaps2(arr) //  // .zipWithIndex.toList

    printWriter.println(res)

    printWriter.close()
  }
}
*/
