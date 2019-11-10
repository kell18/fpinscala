package playground


object Hackerrank extends App {

  case class Observation(birdId: Int, count: Int)

  println(migratoryBirds(Array(1, 2, 3, 4, 5, 4, 3, 2, 1, 3, 4)))

  def migratoryBirds(arr: Array[Int]): Int = {
    migratoryBirds_forSorted(arr.sorted)
  }

  def migratoryBirds_forSorted(sortedArr: Array[Int]): Int = if (sortedArr.nonEmpty) {
    val obs0 = Observation(sortedArr.head, 1)
    val (obsMaxPrev, obsLast) = sortedArr.tail.foldLeft(obs0 -> obs0) {
      case ((maxObs, Observation(birdId, count)), newBirdId) if birdId == newBirdId =>
        maxObs -> Observation(birdId, count + 1)
      case ((maxObs, lastObs), newBirdId) =>
        val newMax = if (maxObs.count > lastObs.count) maxObs
                     else if (maxObs.count < lastObs.count) lastObs
                     // since arr is sorted and we need to return the smallest ID
                     else maxObs
        newMax -> Observation(newBirdId, 1)

    }
    if (obsMaxPrev.count > obsLast.count) obsMaxPrev.birdId
    else if (obsMaxPrev.count < obsLast.count) obsLast.birdId
    else Math.min(obsMaxPrev.birdId, obsLast.birdId)
  }
  else {
    sys.error("Array is empty cannot count Observation.")
  }

  /*println(encryption("haveaniceday"))
  println(encryption("  ha   v e a n  i c e d a y     "))
  println(encryption("feedthedog"))
  println(encryption(" f e e d    t     h e d o g "))
  println(encryption("if man was meant to stay on the ground god would have given us roots"))
  println(encryption("if man was meant to stay on the g r o u n d god would havegivenusroots"))
  println()

  println(encryption(""))
  println(encryption("e"))
  println(encryption("ee"))
  println(encryption("eee"))*/

  def encryption(s: String): String = {
    val L = s.replace(" ", "")
    val sqrtLen = Math.sqrt(L.length)
    val (rows, cols) = Math.floor(sqrtLen).toInt -> Math.ceil(sqrtLen).toInt match {
      case (r, c) if (r * c < L.length) => (r + 1, c)
      case (r, c) => (r, c)
    }
    (0 until cols).map(col =>
      (0 until rows).map { row =>
        val ind = cols * row + col
        if (ind < L.length) L(ind) else ""
      }.mkString("")
    ).mkString(" ")
  }

}
