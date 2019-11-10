package playground

import scala.annotation.tailrec

object Misc extends App {

  private val unitMtx = new RectMatrix(Array(
    Array(1, 0, 0),
    Array(0, -1, 0),
    Array(0, 0, 1)))
  println(unitMtx.isDiagonal)
  println(unitMtx.row(1, 1).mkString(", "))
  println(unitMtx.col(1, 1).mkString(", "))
  println()

  private val test1Mtx = new RectMatrix(Array(
    Array(0, 4, 0),
    Array(2, 0, 2),
    Array(2, 0, 2)))
  println(organizingContainers_(test1Mtx, 0))
  private val test2Mtx = new RectMatrix(Array(
    Array(0, 2, 1),
    Array(1, 1, 1),
    Array(2, 0, 0)))
  println(organizingContainers_(test2Mtx, 0))

  class RectMatrix(val arr: Array[Array[Int]]) {
    val n = arr.length

    def row(ind: Int, exceptInd: Int) = arr(ind).zipWithIndex
      .filter { case (_, i) => i != exceptInd }

    def col(ind: Int, exceptInd: Int) = arr.map(_(ind)).zipWithIndex
      .filter { case (_, i) => i != exceptInd }

    def isDiagonal: Boolean = (0 until n)
      .forall(i => row(i, i).map(_._1).sum == 0 && col(i, i).map(_._1).sum == 0)
  }

  @tailrec
  private def organizingContainers_(mtx: RectMatrix, itemType: Int = 0): String = {
    if (itemType < mtx.n) {
      val containerRow = mtx.row(itemType, exceptInd = itemType)
      val sameTypeCol = mtx.col(itemType, exceptInd = itemType)
      if (containerRow.map(_._1).sum != sameTypeCol.map(_._1).sum) "Impossible"
      else {
        containerRow.foreach {
          case (_, nextItemType) => sameTypeCol.foreach {
            case  (_, nextItemCont) =>
              val currContItems = mtx.arr(itemType)(nextItemType)
              val sameItemTypeItems = mtx.arr(nextItemCont)(itemType)
              val toSwap = Math.min(currContItems, sameItemTypeItems)
              mtx.arr(nextItemCont)(itemType) -= toSwap
              mtx.arr(itemType)(nextItemType) -= toSwap
              mtx.arr(itemType)(itemType) += toSwap
              mtx.arr(nextItemCont)(nextItemType) += toSwap
          }
        }
        organizingContainers_(mtx, itemType + 1)
      }
    } else {
      if (mtx.isDiagonal) "Possible"
      else "Impossible"
    }
  }
}
