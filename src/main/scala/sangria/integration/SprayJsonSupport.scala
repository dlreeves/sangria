package sangria.integration

import sangria.execution.{InputUnmarshaller, ResultMarshaller}
import spray.json._


object SprayJsonSupport extends SprayJsonSupportLowPrioImplicits {

  implicit object SprayJsonResultMarshaller extends ResultMarshaller {
    type Node = JsValue

    def emptyMapNode = JsObject.empty

    def mapNode(keyValues: Seq[(String, JsValue)]) = JsObject(keyValues: _*)
    def mapNodeOpt(keyValues: Seq[(String, Option[JsValue])]) = JsObject(keyValues.collect{case (key, Some(value)) => key -> value}: _*)

    def addMapNodeElem(node: JsValue, key: String, value: JsValue, optional: Boolean) = JsObject(node.asInstanceOf[JsObject].fields + (key -> value))
    def addMapNodeElemOpt(node: JsValue, key: String, value: Option[JsValue], optional: Boolean) =
      value match {
        case Some(v) => JsObject(node.asInstanceOf[JsObject].fields + (key -> v))
        case None => node
      }

    def addArrayNodeElem(array: JsValue, elem: JsValue, optional: Boolean) = JsArray(array.asInstanceOf[JsArray].elements :+ elem)
    def arrayNode(values: Seq[JsValue], optional: Boolean) = JsArray(values.toVector)
    def isEmptyArrayNode(array: JsValue) = array.asInstanceOf[JsArray].elements.isEmpty
    def emptyArrayNode = JsArray.empty

    def toBooleanNode(value: Boolean) = JsBoolean(value)
    def toFloatNode(value: Double) = JsNumber(value)
    def toStringNode(value: String) = JsString(value)
    def toIntNode(value: Int) = JsNumber(value)
    def toBigIntNode(value: BigInt) = JsNumber(value)
    def toBigDecimalNode(value: BigDecimal) = JsNumber(value)
    def toEnumNode(rawValue: String, coercedValue: Any) = JsString(rawValue)

    def fromStringNode(value: JsValue) = value.asInstanceOf[JsString].value
    def fromIntNode(value: JsValue) = value.asInstanceOf[JsNumber].value.intValue()
    def fromBigIntNode(value: JsValue) = value.asInstanceOf[JsNumber].value.toBigInt()
    def fromFloatNode(value: JsValue) = value.asInstanceOf[JsNumber].value.doubleValue()
    def fromBigDecimalNode(value: JsValue) = value.asInstanceOf[JsNumber].value
    def fromBooleanNode(value: JsValue) = value.asInstanceOf[JsBoolean].value
    def fromArrayNode(arrayNode: JsValue) = arrayNode.asInstanceOf[JsArray].elements
    def fromMapNode(mapNode: JsValue) = mapNode.asInstanceOf[JsObject].fields

    def nullNode = JsNull

    def renderCompact(node: JsValue) = node.compactPrint
    def renderPretty(node: JsValue) = node.prettyPrint

    def isNull(node: Node) = node == JsNull
    def isNode(obj: Any) = obj.isInstanceOf[JsValue]

  }

  implicit object SprayJsonInputUnmarshaller extends InputUnmarshaller[JsValue] {
    type LeafNode = JsValue

    def isDefined(node: JsValue) = node != JsNull

    def getScalarValue(node: JsValue) = node match {
      case JsBoolean(b) => b
      case JsNumber(d) => d.toBigIntExact getOrElse d
      case JsString(s) => s
      case _ => throw new IllegalStateException(s"$node is not a scalar value")
    }

    def isScalarNode(node: JsValue) = node match {
      case _: JsBoolean | _: JsNumber | _: JsString => true
      case _ => false
    }

    def isMapNode(node: JsValue) = node.isInstanceOf[JsObject]

    def getListValue(node: JsValue) = node.asInstanceOf[JsArray].elements

    def render(node: JsValue) = node.compactPrint

    def isArrayNode(node: JsValue) = node.isInstanceOf[JsArray]

    def getMapValue(node: JsValue, key: String) = node.asInstanceOf[JsObject].fields get key

    def emptyNode = JsObject.empty

    def getRootMapValue(node: JsValue, key: String) = node.asInstanceOf[JsObject].fields get key

    def getMapKeys(node: JsValue) = node.asInstanceOf[JsObject].fields.keySet
  }
}

trait SprayJsonSupportLowPrioImplicits {
  implicit val SprayJsonInputUnmarshallerJObject =
    SprayJsonSupport.SprayJsonInputUnmarshaller.asInstanceOf[InputUnmarshaller[JsObject]]
}
