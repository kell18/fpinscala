package playground

object DeliveryHero extends App {

  /*
  Example test:    [1, 1]
  WRONG ANSWER  (got [0, 1] expected [-1, -1])

  Example test:    [1, 2]
  WRONG ANSWER  (got [-1, -1] expected [1, 0])

  Example test:    [1, 4]
  WRONG ANSWER  (got [-1, -1] expected [0, 1])

  Example test:    [10, 20]
  WRONG ANSWER  (got [-1, -1] expected [10, 0])

  Example test:    [15, 40]
  WRONG ANSWER  (got [-1, -1] expected [10, 5])

  Example test:    [7, 16]
  WRONG ANSWER  (got [-1, -1] expected [6, 1])
  */


  def optimalBurgers(cheese: Int, tomatoes: Int) =
    if (tomatoes % 2 != 0) Array(-1, -1)
    else if (tomatoes == cheese * 2) Array(0, cheese)
    else if ((tomatoes % 4) % 2 == 0) Array(tomatoes / 4, (tomatoes % 4) / 2)
    else if (cheese > tomatoes) Array(-1, -1)
    else if (cheese == tomatoes) Array(0, cheese)
    else {
      val big = tomatoes / 4
      val small = tomatoes - big
      Array(big, small)
    }

  def maxRun(a: Array[Int]): Int = if (a.length < 2) a.length else {
    val (totalMaxRun, (prevRun, _)) = a.tail.foldLeft(1 -> (1, a.head)) {
      case ((maxRun, (currRun, prevNum)), currNum) if Math.abs(currNum - prevNum) == 1 =>
        Math.max(maxRun, currRun + 1) -> (currRun + 1, currNum)

      // currNum doesn't belongs to currRun
      case ((maxRun, (currRun, prevNum)), currNum) => Math.max(maxRun, currRun) -> (1, currNum)
    }
    Math.max(totalMaxRun, prevRun)
  }


  // println(solution(Array(3, 2, 3, 2, 3)))
  // println(solution1(Array(7, 4, -2, 4, -2, -9)))


  def solution1(a: Array[Int]): Int = {
    println(a.mkString(","))
    maxSwitch(a)
  }

  sealed trait Slice extends Product with Serializable {
    def length: Int
  }
  object Slice {
    def empty: Slice = EmptySlice
  }

  case class HalfSlice(num: Int, ind: Int) extends Slice {
    override def length = 1
  }

  case class FullSlice(even: Int, odd: Int, length: Int) extends Slice {
    def isBelongsToSlice(num: Int, ind: Int) = if (ind % 2 == 0) num == this.even else num == this.odd

    def increaseLength: FullSlice = this.copy(length = length + 1)
  }

  case object EmptySlice extends Slice {
    override def length = 0
  }

  def maxSwitch(a: Array[Int]): Int = if (a.length < 3) a.length else {
    val (prevMaxLen, lastSlice) = a.zipWithIndex.foldLeft(0 -> Slice.empty) {
      case ((maxSliceLen, EmptySlice), (num, ind)) =>
        Math.max(maxSliceLen, 1) -> HalfSlice(num, ind)

      case ((maxSliceLen, HalfSlice(sNum, sInd)), (num, ind)) =>
        Math.max(maxSliceLen, 2) -> (
          if (sInd % 2 == 0) FullSlice(sNum, num, 2) else FullSlice(num, sNum, 2)
        )

      case ((maxSliceLen, slice@FullSlice(even, odd, sLen)), (num, ind)) if slice.isBelongsToSlice(num, ind) =>
        if (slice.length + 1 > maxSliceLen) (sLen + 1) -> slice.increaseLength
        else maxSliceLen -> slice.increaseLength

      // new num does't belongs to current slice
      case ((maxSliceLen, slice@FullSlice(even, odd, sLen)), (num, ind)) =>
        Math.max(maxSliceLen, 2) -> (
          if (ind % 2 == 0) FullSlice(num, odd, 2) else FullSlice(odd, num, 2)
        )

      case x => sys.error(s"something went wrong: $x")
    }
    Math.max(prevMaxLen, lastSlice.length)
  }

}
