package playground

import scala.annotation.tailrec


// val l = List(234, 1198, 99, 2341, 999, 811, 2341, 2342, 2343, 2344, 2345, 2346, 2347, 2348, 2349, 987, 7777, 55, 257, 27).avg

// val l1 = List(2340, 1198, 9009, 2341, 9990, 8011, 2341, 2342, 2343, 2344, 2345, 2346, 2347, 2348, 2349, 9087, 7777, 5500, 2570, 2007)



object StacksAndQueues extends App {

  /*println(doFishMassacre(2 :: 3 :: 4 :: 6 :: Nil, 5))
  println(doFishMassacre(2 :: 3 :: 4 :: 6 :: Nil, 10))
  println(doFishMassacre(2 :: 3 :: 4 :: 6 :: Nil, 1))
  println(doFishMassacre(Nil, 1))
  println()*/

  /*println(fish(Array(
    4 -> 0,
    3 -> 1
  )))
  println(fish(Array(
    4 -> 0,
    3 -> 1,
    2 -> 0,
    1 -> 0,
    5 -> 0
  )))
  println()

  println(fish(Array(
    4 -> 1,
    3 -> 1,
    2 -> 0,
    1 -> 0,
    5 -> 0
  )))
  println()

  println(fish(Array(4 -> 1)))*/

  /*val bigFishDown = (1 to 100000).reverse.map(_ -> 1).toArray
  println(fish(bigFishDown))
  val bigFishDown1Up = bigFishDown :+ (100001 -> 0)
  println(fish(bigFishDown1Up))
  val bigFishDownManyUp = bigFishDown ++ Array(100001 -> 1, 100002 -> 0, 100003 -> 1)
  println(fish(bigFishDownManyUp))*/

  val FlowingUp = 0
  val FlowingDown = 1

  def fish_(a: Array[Int], b: Array[Int]): Int = fish(a.zip(b))

  def fish(sizesAndDirections: Array[(Int, Int)]): Int = {
    val (flowDown, flowUp) = sizesAndDirections.foldLeft(List.empty[Int] -> 0) {
      case ((flowDownSizes, flowUpIntact), (size, FlowingUp)) => doFishMassacre(flowDownSizes, size) match {
        // if nothing flowing down or all of them has been eaten - increase flowUpIntact
        case Nil => Nil -> (flowUpIntact + 1)
        // if some of the fish flowing down has survived else - the fish flowing up has been eaten
        case something => something -> flowUpIntact
      }
      case ((flowDownSizes, flowUpIntact), (size, FlowingDown)) => (size :: flowDownSizes) -> flowUpIntact

      case (_, (_, unknown)) => sys.error(s"Invalid fish direction: $unknown")
    }

    flowDown.length + flowUp
  }

  def doFishMassacre(flowDownSizes: List[Int], flowUpSize: Int): List[Int] = flowDownSizes.dropWhile(_ < flowUpSize)

  /*println(nestedCheck(""))
  println(nestedCheck("{}"))
  println(nestedCheck("{[[[]]]}"))
  println(nestedCheck("{[()()]}"))
  println(nestedCheck("(){[()([])]}"))
  println()

  println(nestedCheck("([)()]"))
  println(nestedCheck("([(]"))
  println(nestedCheck("("))
  println(nestedCheck(")"))
  println(nestedCheck("]}))))))))))))"))
  println(nestedCheck("[[[[[[[[{{{{(({{[["))
  println(nestedCheck("([(0]"))
  println()*/

  /*val bigS_correct = (1 to 100000).map(_ => '(') ++ (1 to 100000).map(_ => ')')
  println(nestedCheck(bigS_correct.mkString("")))
  val bigS_invalid = (1 to 100000).map(_ => '(') ++ (1 to 100000).map(_ => ']')
  println(nestedCheck(bigS_invalid.mkString("")))*/

  def nestedCheck(str: String): Int = {
    @tailrec
    def isNested(strInd: Int, opens: List[OpenBracket]): Boolean = {
      if (strInd == str.length) opens.isEmpty
      else {
        val currChar = str(strInd)
        OpenBracket.fromChar(currChar) match {
          case Some(open) => isNested(strInd + 1, open :: opens)
          case None => opens match {
            case opened :: restOpens =>
              if (OpenBracket.isMatchingPair(opened, currChar)) isNested(strInd + 1, restOpens) else false
            case Nil => false
          }
        }
      }
    }

    if (isNested(0, Nil)) 1 else 0
  }



  sealed trait OpenBracket extends Product with Serializable
  case object Round extends OpenBracket
  case object Curly extends OpenBracket
  case object Square extends OpenBracket

  object OpenBracket {
    def fromChar(c: Char): Option[OpenBracket] = c match {
      case '(' => Some(Round)
      case '{' => Some(Curly)
      case '[' => Some(Square)
      case _ => None
    }

    def isMatchingPair(open: OpenBracket, closing: Char): Boolean = open match {
      case Round => closing == ')'
      case Curly => closing == '}'
      case Square => closing == ']'
    }
  }
}
