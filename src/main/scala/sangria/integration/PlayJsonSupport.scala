package sangria.integration

import play.api.libs.json._
import sangria.execution.{InputUnmarshaller, ResultMarshaller}

object PlayJsonSupport extends PlayJsonSupportLowPrioImplicits {
  implicit object PlayJsonResultMarshaller extends ResultMarshaller {
    type Node = JsValue

    def emptyMapNode = JsObject(Seq.empty)

    def mapNode(keyValues: Seq[(String, JsValue)]) = JsObject(keyValues)
    def mapNodeOpt(keyValues: Seq[(String, Option[JsValue])]) = JsObject(keyValues collect {case (key, Some(value)) => key -> value})

    def addMapNodeElem(node: JsValue, key: String, value: JsValue, optional: Boolean) = node.asInstanceOf[JsObject] + (key -> value)
    def addMapNodeElemOpt(node: JsValue, key: String, value: Option[JsValue], optional: Boolean) =
      value match {
        case Some(v) => node.asInstanceOf[JsObject] + (key -> v)
        case None => node
      }

    def isEmptyArrayNode(array: JsValue) = array.asInstanceOf[JsArray].value.isEmpty
    def emptyArrayNode = JsArray(Seq.empty)
    def addArrayNodeElem(array: JsValue, elem: JsValue, optional: Boolean) = array.asInstanceOf[JsArray].append(elem)
    def arrayNode(values: Seq[JsValue], optional: Boolean) = JsArray(values)

    def toBooleanNode(value: Boolean) = JsBoolean(value)
    def toFloatNode(value: Double) = JsNumber(value)
    def toStringNode(value: String) = JsString(value)
    def toIntNode(value: Int) = JsNumber(value)
    def toBigIntNode(value: BigInt) = JsNumber(BigDecimal(value))
    def toBigDecimalNode(value: BigDecimal) = JsNumber(value)
    def toEnumNode(rawValue: String, coercedValue: Any) = JsString(rawValue)

    def fromStringNode(value: JsValue) = value.asInstanceOf[JsString].value
    def fromIntNode(value: JsValue) = value.asInstanceOf[JsNumber].value.intValue()
    def fromBigIntNode(value: JsValue) = value.asInstanceOf[JsNumber].value.toBigInt()
    def fromFloatNode(value: JsValue) = value.asInstanceOf[JsNumber].value.doubleValue()
    def fromBigDecimalNode(value: JsValue) = value.asInstanceOf[JsNumber].value
    def fromBooleanNode(value: JsValue) = value.asInstanceOf[JsBoolean].value
    def fromArrayNode(arrayNode: JsValue) = arrayNode.asInstanceOf[JsArray].value
    def fromMapNode(mapNode: JsValue) = mapNode.asInstanceOf[JsObject].value.toMap

    def nullNode = JsNull

    def renderCompact(node: JsValue) = Json.stringify(node)
    def renderPretty(node: JsValue) = Json.prettyPrint(node)

    def isNull(node: JsValue) = node == JsNull
    def isNode(obj: Any) = obj.isInstanceOf[JsValue]
  }

  implicit object PlayJsonInputUnmarshaller extends InputUnmarshaller[JsValue] {
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

    def getListValue(node: JsValue) = node.asInstanceOf[JsArray].value

    def render(node: JsValue) = Json.stringify(node)

    def isArrayNode(node: JsValue) = node.isInstanceOf[JsArray]

    def getMapValue(node: JsValue, key: String) = node.asInstanceOf[JsObject].value get key

    def emptyNode = JsObject(Seq.empty)

    def getRootMapValue(node: JsValue, key: String) = node.asInstanceOf[JsObject].value get key

    def getMapKeys(node: JsValue) = node.asInstanceOf[JsObject].keys
  }
}

trait PlayJsonSupportLowPrioImplicits {
  implicit val SprayJsonInputUnmarshallerJObject =
    PlayJsonSupport.PlayJsonInputUnmarshaller.asInstanceOf[InputUnmarshaller[JsObject]]
}
