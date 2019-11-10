package playground

object GenomicRangeQuery extends App {

  // println(solution("CAGCCTA", Array(2, 5, 0), Array(4, 5, 6)).mkString(", "))
  // println(solution("C", Array(0, 0), Array(0, 0)).mkString(", "))
  // println(solution("C", Array(), Array()).mkString(", "))

  case class GenomesPrefSum(prefSums: List[Array[Int]]) {
    def minImpactInRange(fromInd: Int, toInd: Int): Int = {
      val firstImpactInd =
        if (fromInd < toInd) prefSums.zipWithIndex.find { case (gen, _) => gen(toInd) - gen(fromInd) > 0 }
        else if (fromInd > 0) prefSums.zipWithIndex.find { case (gen, _) => gen(fromInd) - gen(fromInd-1) > 0 }
        else prefSums.zipWithIndex.find { case (gen, _) => gen(fromInd) > 0 }

      firstImpactInd
        .map(_._2 + 1)
        .getOrElse(sys.error(s"Invalid genomes: $prefSums"))
    }
  }
  object GenomesPrefSum {
    val inToIndexMap = Map('A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)

    def empty(n: Int) = GenomesPrefSum(List.fill(inToIndexMap.size)(Array.fill(n)(0)))

    def fromString(s: String) = {
      val genomes = empty(s.length)
      s.zipWithIndex.foreach {
        case (gen, i) =>
          genomes.prefSums(inToIndexMap(gen))(i) = if (i > 0) genomes.prefSums(inToIndexMap(gen))(i - 1) + 1 else 1
      }
      genomes
    }
  }

  def solution(s: String, p: Array[Int], q: Array[Int]): Array[Int] = {
    val prefSums = GenomesPrefSum.fromString(s)
    p.zip(q).map {
      case (pInd, qInd) => prefSums.minImpactInRange(pInd, qInd)
    }
  }

}


/*

CAGCCTA


2 1 3 2 2 4 1


2 4
5 5
0 6



*/