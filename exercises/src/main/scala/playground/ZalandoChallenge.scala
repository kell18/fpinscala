package playground

object ZalandoChallenge extends App {

  /*println(Math.sqrt(11))
  println(Math.sqrt(30))
  println(Math.sqrt(6))
  println(Math.sqrt(12))
  println(Math.sqrt(20))

  for (i <- 1 to 1) {
    val mult = i * (i + 1L)
    if (Math.ceil(Math.sqrt(mult)).toLong > i) {
      println(s"wrong for $i with $mult and ${Math.ceil(Math.sqrt(mult))}")
    }
  }*/

  /*println(multiplicators(21, 29))
  println(multiplicators(6, 20))
  println(multiplicators(30, 35))
  println(multiplicators(312, 320))

  println(multiplicators(1, 1))
  println(multiplicators(2, 2))
  println(multiplicators(6, 6))
  println(multiplicators(1, 100))
  println(multiplicators(1, 1000))
  println(multiplicators(1, 10000))
  println(multiplicators(1, 100000))
  println(multiplicators(1, 1000000))
  println(multiplicators(1, 10000000))
  println(multiplicators(1, 100000000))
  println(multiplicators(1, 1000000000))*/

  /*
  0
  1
  1
  9
  31
  99
  315
  999
  3161
  9999
  31622
  Привет! Блин, на этой неделе всё забито, волик и встречи вечером

   */

  println(multiplicators(1, 1))
  println(multiplicators(100, 100))
  println(multiplicators(100, 100))
  println(multiplicators(100000000, 100000001))
  println(multiplicators(100000000, 1000000010))

  def multiplicators(a: Int, b: Int): Int = {
    // We traverse numbers only in a range of square roots of a and b because
    // we know that if the number could be factorized to two increasing numbers
    // it'd be 1 unit away from the square root of the number (if exists at all).
    val firstFloor = Math.sqrt(a).floor.toInt
    val lastCeil = (Math.sqrt(b) + 1.0).floor.toInt
    (firstFloor to lastCeil).count { i =>
      val m = i * (i + 1)
      m >= a && m <= b
    }
  }

  def multiplicators1(a: Int, b: Int): Int = {
    (a to b).count { i =>
      val sqrt = Math.sqrt(i)
      val m1 = sqrt.floor.toInt
      val m2 = sqrt.ceil.toInt
      m1 * m2 == i && m1 != m2
    }
  }

  /*println(Math.pow(10, 0))
  println(Math.pow(10, 1))
  println(Math.pow(10, 2))
  println(Math.pow(10, 8).toInt)*/

  /*println(smallestNum(0))
  println(smallestNum(1))
  println(smallestNum(9))
  println()

  println(smallestNum(10))
  println(smallestNum(99))
  println()

  println(smallestNum(100))
  println(smallestNum(101))
  println(smallestNum(999))
  println(smallestNum(745))
  println()

  println(smallestNum(9809))
  println(smallestNum(45235))
  println(smallestNum(985467))
  println(smallestNum(9854674))
  println(smallestNum(98546742))
  println(smallestNum(985467423))
  println(smallestNum(1985467423))*/

  def smallestNum1(n: Int): Int = {
    val power10 = Math.log10(n).floor.toInt
    if (power10 > 0) Math.pow(10, power10).toInt else 0
  }

  def smallestNum(n: Int): Int = {
    val numDigits = n.toString.length
    if (numDigits > 1) Math.pow(10, numDigits - 1).toInt else 0
  }
}
