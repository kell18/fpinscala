package playground

import scala.annotation.tailrec
import scala.collection.mutable

object Dictionaries extends App {

  println(fib(5))

  def fib(n: Int, i: Int = 0): Long = {
    if (n == 0) 0
    else if (n <= 2) 1
    else {
      i + fib(n - 1, i + 1)
    }
  }

  /*println(makeAnagram("abc", "cde")) // 4
  println(makeAnagram("abc", "abc")) // 0
  println(makeAnagram("abc", "cba")) // 0
  println(makeAnagram("qwe", "abc")) // 6
  println(makeAnagram("abccc", "cccde")) // 4

  println(makeAnagram("abbcc", "ccdbb")) // 2*/

  def makeAnagram(a: String, b: String): Int = {
    val mapA = a.foldLeft(Map.empty[Char, Int]) {
      case (map, ac) => map + (ac -> map.getOrElse(ac, 1))
    }
    val (uniqB, sameBA) = b.span(mapA.contains)
    val uniqA = mapA.filterKeys(!sameBA.contains(_))
    uniqA.values.sum + uniqB.length
  }

  def makeAnagram1(a: String, b: String): Int = {
    a.diff(b).length + b.diff(a).length
  }

  /*println(sherlockAndAnagrams("")) // 0
  println(sherlockAndAnagrams("a")) // 0
  println(sherlockAndAnagrams("abcde")) // 0
  println(sherlockAndAnagrams("abba")) // 4
  println(sherlockAndAnagrams("kkkk")) // 10
  println(sherlockAndAnagrams("ifailuhkqq")) // 3
  println(sherlockAndAnagrams("iail")) // 2*/
  // println(sherlockAndAnagrams("iaili"))
  /*
  iail
  ail
  il
  l
  iai
  ai
  i
  ia
  a
  i
  */

  def sherlockAndAnagrams(s: String): Int = {
    val reverseStrings = buildReverseMap(s)
    val directStrings = adjacentSubstringsIndexes(s)
    directStrings.foldLeft(0) {
      case (sum, (directS, directInd)) =>
        sum + reverseStrings.get(directS).map(_.count(_ > directInd)).getOrElse(0)
    }
  }

  def buildReverseMap(str: String): collection.Map[String, List[Int]] = {
    adjacentSubstringsIndexes(str)
      .foldLeft(Map.empty[String, List[Int]]) {
        case (map, (s, ind)) => map + ((s, ind :: map.getOrElse(s, Nil)))
      }
  }


  def adjacentSubstringsIndexes(str: String): List[(String, Int)] = {
    @tailrec
    def go(s: String, substrings: List[(String, Int)]): List[(String, Int)] = {
      val tailsAndInds = s.tails.toList.map(_.sorted).zip(0 until s.length)
      if (s.isEmpty) substrings else go(s.dropRight(1),  substrings ++ tailsAndInds)
    }

    go(str, List()).filter(_._1 != "")
  }

  /*
  aabb
  abb
  ab
  a
  abb
  bb
  b
  ab
  b
  a
  */

  /*
  abcde
  bcde
  cde
  de
  e
  bcd
  cd
  d
  bc
  c
  b


  println(adjacentSubstringsIndexes("abcde"))
  println(adjacentSubstringsIndexes("abba"))
  println(adjacentSubstringsIndexes("a"))
  println(adjacentSubstringsIndexes(""))
  */

}
