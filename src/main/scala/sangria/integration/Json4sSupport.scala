package sangria.integration

import org.json4s.JsonAST._
import sangria.execution.{InputUnmarshaller, ResultMarshaller}

import org.json4s.native.JsonMethods.{render => jsonRender, _}

object Json4sSupport extends Json4sSupportLowPrioImplicits {
  
  implicit object Json4sResultMarshaller extends ResultMarshaller {
    type Node = JValue

    def emptyMapNode = JObject(Nil)

    def mapNode(keyValues: Seq[(String, JValue)]) = JObject(keyValues.toList)
    def mapNodeOpt(keyValues: Seq[(String, Option[JValue])]) = JObject(keyValues.collect{case (key, Some(value)) => key -> value}.toList)

    def addMapNodeElem(node: JValue, key: String, value: JValue, optional: Boolean) =
      JObject(node.asInstanceOf[JObject].obj :+ (key -> value))
    def addMapNodeElemOpt(node: Node, key: String, value: Option[JValue], optional: Boolean) = value match {
      case Some(v) => JObject(node.asInstanceOf[JObject].obj :+ (key -> v))
      case None => node
    }

    def addArrayNodeElem(array: JValue, elem: JValue, optional: Boolean) = JArray(array.asInstanceOf[JArray].arr :+ elem)
    def arrayNode(values: Seq[JValue], optional: Boolean) = JArray(values.toList)
    def isEmptyArrayNode(array: JValue) = array.asInstanceOf[JArray].arr.isEmpty
    def emptyArrayNode = JArray(Nil)

    def toBooleanNode(value: Boolean) = JBool(value)
    def toFloatNode(value: Double) = JDouble(value)
    def toStringNode(value: String) = JString(value)
    def toIntNode(value: Int) = JInt(value)
    def toBigIntNode(value: BigInt) = JInt(value)
    def toBigDecimalNode(value: BigDecimal) = JDecimal(value)
    def toEnumNode(rawValue: String, coercedValue: Any) = JString(rawValue)

    def fromStringNode(value: JValue) = value.asInstanceOf[JString].s
    def fromIntNode(value: JValue) = value.asInstanceOf[JInt].num.intValue()
    def fromBigIntNode(value: JValue) = value.asInstanceOf[JInt].num
    def fromFloatNode(value: JValue) = value.asInstanceOf[JDouble].num
    def fromBigDecimalNode(value: JValue) = value.asInstanceOf[JDecimal].num
    def fromBooleanNode(value: JValue) = value.asInstanceOf[JBool].value
    def fromArrayNode(arrayNode: JValue) = arrayNode.asInstanceOf[JArray].arr
    def fromMapNode(mapNode: JValue) = mapNode.asInstanceOf[JObject].obj.toMap
    
    def nullNode = JNull

    def renderCompact(node: JValue) = compact(jsonRender(node))
    def renderPretty(node: JValue) = pretty(jsonRender(node))

    def isNull(node: JValue) = node == JNull
    def isNode(obj: Any) = obj.isInstanceOf[JValue]
  }

  implicit object Json4sInputUnmarshaller extends InputUnmarshaller[JValue] {
    type LeafNode = JValue

    def isDefined(node: JValue) = node != JNull && node != JNothing

    def getScalarValue(node: JValue) = node match {
      case JBool(b) => b
      case JInt(i) => i
      case JDouble(d) => d
      case JDecimal(d) => d
      case JString(s) => s
      case _ => throw new IllegalStateException(s"$node is not a scalar value")
    }

    def isScalarNode(node: JValue) = node match {
      case _: JBool | _: JNumber | _: JString => true
      case _ => false
    }

    def isMapNode(node: JValue) = node.isInstanceOf[JObject]

    def getListValue(node: JValue) = node.asInstanceOf[JArray].arr

    def render(node: JValue) = compact(jsonRender(node))

    def isArrayNode(node: JValue) = node.isInstanceOf[JArray]

    def getMapValue(node: JValue, key: String) = node.asInstanceOf[JObject].obj.find(_._1 == key).map(_._2)

    def emptyNode = JObject()

    def getRootMapValue(node: JValue, key: String) = node.asInstanceOf[JObject].obj.find(_._1 == key).map(_._2)

    def getMapKeys(node: JValue) = node.asInstanceOf[JObject].values.keySet
  }
}

trait Json4sSupportLowPrioImplicits {
  implicit val Json4sInputUnmarshallerJObject =
    Json4sSupport.Json4sInputUnmarshaller.asInstanceOf[InputUnmarshaller[JObject]]
}
