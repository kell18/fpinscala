package playground

object Sorting extends App {

  import scala.collection.Searching._
  /*val a = Array(-4, -1, 0, 0, 2, 5)
  val a1 = Array(3, 4, 5, 7, 8, 9)
  println(a.search(-4))*/

  // println(discs(Array(1, 5, 2, 1, 4, 0)))
  println(findIntersectionsOrNeg1(Array(-1 -> 1, 1 -> 3, 3 -> 5, 1 -> 11))) // 12 -> 14



  def discs(a: Array[Int]): Int = {
    val coords = a.zipWithIndex
      .map {
        case (rad, ind) => (ind - rad, ind + rad)
      }
      .sortBy(_._1)
    println(coords.mkString(", "))


    findIntersectionsOrNeg1(coords)
  }

  def findIntersectionsOrNeg1(coords: Array[(Int, Int)]): Int = {
    val starts = coords.map(_._1)
    println(starts.mkString(", "))

    var totalIntersects = 0L
    for (ind <- coords.indices) {
      val end = coords(ind)._2
      // search maximum index of element which starts is closest to the end of current element
      // and then get the length of slice which inside current disc
      totalIntersects += (starts.search(end) match {
        case result if result.insertionPoint <= ind =>
          println(s"$end not found $ind")
          0

        case Found(lastInd) =>
          val t = lastInd - ind
          val sameNext =
            if (lastInd < coords.length - 1) starts.view(lastInd, coords.length).takeWhile(_ == end).length else 0
          t + sameNext
        case InsertionPoint(lastExclusiveInd) =>
          val t = lastExclusiveInd - 1 - ind
          t
      })
      println(totalIntersects)
      if (totalIntersects > 10000000)
        return -1
    }
    if (totalIntersects > 10000000) -1 else totalIntersects.toInt
  }

  /*println(distinct(Array(1, -100, 32421, 112, -3456345, 132, 432, 432, -100, 112)))
  println(distinct(Array.fill(100000)(-1000)))
  println(distinct(Array.fill(100000)(-1000) :+ 1000))*/

  def distinct(a: Array[Int]): Int = {
    a.distinct.length
  }

  /*println(maxProductOfThree(Array(12, 14, 754, 12, 867, 12, 76)))
  println(maxProductOfThree(Array.fill(100000)(-1000) :+ 1000))
  println(maxProductOfThree(Array.fill(100000)(1000)))
  val a = (1 to 100000).map(_ % 2000 - 1000).reverse.toArray
  println(a.sorted.takeRight(3).mkString(", "))
  println(maxProductOfThree(a))*/

  def maxProductOfThree(a: Array[Int]): Int = {
    val sorted = a.sorted
    val last3 = sorted.takeRight(3)
    val first2 = sorted.take(2)
    Math.max(
      last3.product,
      first2.product * last3.last
    )
  }

}
