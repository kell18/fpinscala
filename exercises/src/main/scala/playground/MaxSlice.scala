package playground

object MaxSlice extends App {

  println(solution(Array(5, 7, 9, 7, 8, 11, 6, 12)))
  println(solution(Array(10, 8, 9, 5, 7, 9, 12, 10, 7, 3, 5)))
  println(solution(Array(5, 7, 9, 12)))
  println()

  println(solution(Array(5, 7, 9, 12).reverse))
  println(solution(Array(12)))
  println(solution(Array(12, 7)))
  println(solution(Array()))
  println()

  val bigSpike = 1 to 200000
  println(solution(bigSpike.toArray[Int] ++ bigSpike.reverse.toArray[Int]))

  case class PriceSlice(min: Int, maxProfit: Int)

  def solution(a: Array[Int]): Int = if (a.length < 2) 0 else {
    val PriceSlice(_, totalMaxProfit) = a.tail.foldLeft(PriceSlice(a.head, 0)) {
      case (p@PriceSlice(min, _), price)         if price < min             => p.copy(min = price)
      case (p@PriceSlice(min, maxProfit), price) if price - min > maxProfit => p.copy(maxProfit = price - min)

      case (someSlice, _) => someSlice
    }
    totalMaxProfit
  }
}
