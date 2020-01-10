/*
package playground

import pprint.{PPrinter, Tree, Util}

object PrintUtils {
  // in scala 2.13 this would be even simpler/cleaner due to added product.productElementNames
  protected def caseClassToMap(cc: Product): Map[String, Any] = {
    val fieldValues = cc.productIterator.toSet
    val fields = cc.getClass.getDeclaredFields.toSeq
      .filterNot(f => f.isSynthetic || java.lang.reflect.Modifier.isStatic(f.getModifiers))
    fields.map { f =>
      f.setAccessible(true)
      f.getName -> f.get(cc)
    }.filter { case (k, v) => fieldValues.contains(v) }
      .toMap
  }

  protected def pprintAdditionalHandlers: PartialFunction[Any, Tree] = {
    case x: Product =>
      val className = x.getClass.getName
      // see source code for pprint.treeify()
      val shouldNotPrettifyCaseClass =
        x.productArity == 0 || (x.productArity == 2 && Util.isOperator(x.productPrefix)) ||
          className.startsWith(pprint.tuplePrefix) || className == "scala.Some"
      if (shouldNotPrettifyCaseClass)
        pprint.treeify(x)
      else {
        val fieldMap = caseClassToMap(x)
        pprint.Tree.Apply(
          x.productPrefix,
          fieldMap.iterator.flatMap { case (k, v) =>
            val prettyValue: Tree = pprintAdditionalHandlers.lift(v).getOrElse(pprint2.treeify(v))
            Seq(pprint.Tree.Infix(Tree.Literal(k), "=", prettyValue))
          }
        )
      }
  }

  val pprint2 = pprint.copy(additionalHandlers = pprintAdditionalHandlers)
}

case class Sub()
case object SubObj
case class Abc(q: Int, w: String, e: Option[Long], n: Object, sub: Sub, subObj: SubObj.type)
case class AbcT[T](q: T, w: String, e: Option[Long], n: Object, sub: Sub, subObj: SubObj.type)

class AbcC(val q: Int, val w: String, e: Option[Long], val n: Object, sub: Sub, val subObj: SubObj.type)

object Printing extends App {
  val abc = Abc(1, "2", Some(3), null, Sub(), SubObj)
  val nulls = Abc(1, null, null, null, null, null)
  PrintUtils.pprint2.pprintln(abc)
  PrintUtils.pprint2.pprintln(nulls)

  val abcC = new AbcC(1, "2", Some(3), null, Sub(), SubObj)
  PrintUtils.pprint2.pprintln(abcC)

  val abcT = AbcT[Abc](abc, "2", Some(3), null, Sub(), SubObj)
  val abcTC = AbcT[AbcC](abcC, "2", Some(3), null, Sub(), SubObj)
  PrintUtils.pprint2.pprintln(abcT)
  PrintUtils.pprint2.pprintln(abcTC)

}
*/
