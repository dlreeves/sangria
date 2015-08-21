package sangria.execution

trait ResultMarshaller {
  type Node

  def emptyMapNode: Node
  def addMapNodeElemOpt(node: Node, key: String, value: Option[Node], optional: Boolean = false): Node
  def addMapNodeElem(node: Node, key: String, value: Node, optional: Boolean = false): Node
  def mapNodeOpt(keyValues: Seq[(String, Option[Node])]): Node
  def mapNode(keyValues: Seq[(String, Node)]): Node

  def emptyArrayNode: Node
  def arrayNode(values: Seq[Node], optional: Boolean = false): Node
  def addArrayNodeElem(array: Node, elem: Node, optional: Boolean = false): Node
  def isEmptyArrayNode(array: Node): Boolean

  def toStringNode(value: String): Node
  def toIntNode(value: Int): Node
  def toBigIntNode(value: BigInt): Node
  def toFloatNode(value: Double): Node
  def toBigDecimalNode(value: BigDecimal): Node
  def toBooleanNode(value: Boolean): Node
  def toEnumNode(rawValue: String, coercedValue: Any): Node

  def fromStringNode(value: Node): String
  def fromIntNode(value: Node): Int
  def fromBigIntNode(value: Node): BigInt
  def fromFloatNode(value: Node): Double
  def fromBigDecimalNode(value: Node): BigDecimal
  def fromBooleanNode(value: Node): Boolean
  def fromArrayNode(arrayNode: Node): Seq[Node]
  def fromMapNode(mapNode: Node): Map[String, Node]
  
  def nullNode: Node

  def renderCompact(node: Node): String
  def renderPretty(node: Node): String
  
  def isNode(obj: Any): Boolean
  def isNull(node: Node): Boolean
}

object ResultMarshaller {
  implicit val defaultResultMarshaller = new ScalaResultMarshaller
  case object Undefined
}

class ScalaResultMarshaller extends ResultMarshaller {
  type Node = Any

  def toBooleanNode(value: Boolean) = value
  def toFloatNode(value: Double) = value
  def toStringNode(value: String) = value
  def toIntNode(value: Int) = value
  def toBigIntNode(value: BigInt) = value
  def toBigDecimalNode(value: BigDecimal) = value
  def toEnumNode(rawValue: String, coercedValue: Any) = rawValue
  
  def fromStringNode(value: Any) = value.asInstanceOf[String]
  def fromIntNode(value: Any) = value.asInstanceOf[Int]
  def fromBooleanNode(value: Any) = value.asInstanceOf[Boolean]
  def fromFloatNode(value: Any) = value.asInstanceOf[Double]
  def fromBigDecimalNode(value: Any) = value.asInstanceOf[BigDecimal]
  def fromBigIntNode(value: Any) = value.asInstanceOf[BigInt]
  def fromArrayNode(arrayNode: Any) = arrayNode.asInstanceOf[Seq[Any]]
  def fromMapNode(mapNode: Any) = mapNode.asInstanceOf[Map[String, Any]]

  def arrayNode(values: Seq[Node], optional: Boolean) = values
  def emptyArrayNode = Nil
  def addArrayNodeElem(array: Node, elem: Node, optional: Boolean) = array.asInstanceOf[List[_]] :+ elem
  def isEmptyArrayNode(array: Node) = array.asInstanceOf[List[_]].isEmpty

  def mapNodeOpt(keyValues: Seq[(String, Option[Node])]) = Map(keyValues collect {case (key, Some(value)) => key -> value}: _*)
  def mapNode(keyValues: Seq[(String, Node)]) = Map(keyValues: _*)
  def emptyMapNode = Map.empty[String, Any]
  def addMapNodeElemOpt(node: Node, key: String, value: Option[Node], optional: Boolean) = value match {
    case Some(v) => node.asInstanceOf[Map[String, Any]] + (key -> v)
    case None => node
  }
  def addMapNodeElem(node: Node, key: String, value: Node, optional: Boolean) =
    node.asInstanceOf[Map[String, Any]] + (key -> value)

  override def nullNode = null

  def renderCompact(node: Any) = "" + node
  def renderPretty(node: Any) = "" + node

  def isNode(obj: Any) = obj match {
    case _: Boolean | _: Double | _: String | _: Int | _: BigInt | _: BigDecimal | _: Seq[_] |_: Map[_, _] | null => true
    case _ => false
  }

  def isNull(node: Node) = node == null
}
