package sangria.schema

import sangria.execution.{ScalaResultMarshaller, ResultMarshaller}

trait InputTypeUnmarshaller[T, RM <: ResultMarshaller] {
  type Result

  def marshaller: RM
  def unmarshal(marshaller: RM)(node: Option[marshaller.Node]): Result
}

object InputTypeUnmarshaller {
  class OptionUnmarshaller[T, RM <: ResultMarshaller](val ity: InputTypeUnmarshaller[T, RM]) extends InputTypeUnmarshaller[Option[T], RM] {
    type Result = Option[ity.Result]

    override def marshaller = ity.marshaller
    override def unmarshal(marshaller: RM)(node: Option[marshaller.Node]) =
      node flatMap (n => if (node == null) None else Some(ity.unmarshal(marshaller)(Some(n))))
  }

  class SeqUnmarshaller[T, RM <: ResultMarshaller](val ity: InputTypeUnmarshaller[T, RM]) extends InputTypeUnmarshaller[Seq[T], RM] {
    type Result = Seq[ity.Result]

    override def marshaller = ity.marshaller
    override def unmarshal(marshaller: RM)(node: Option[marshaller.Node]) =
      marshaller.fromArrayNode(node.get).map(n => ity.unmarshal(marshaller)(Some(n))) // todo!?
  }

  implicit object MapUnmarshaller extends InputTypeUnmarshaller[Map[String, Any], InputScalaResultMarshaller.type] {
    type Result = Map[String, Any]

    override def marshaller = InputScalaResultMarshaller
    override def unmarshal(marshaller: InputScalaResultMarshaller.type)(node: Option[marshaller.Node]) =
      marshaller.fromMapNode(node.get)
  }

  implicit object ArgsUnmarshaller extends SimpleUnmarshaller[Args](node => Args(InputScalaResultMarshaller.fromMapNode(node)))

  class EnumUnmarshaller[T] extends InputTypeUnmarshaller[EnumInput[T], InputScalaResultMarshaller.type] {
    type Result = T

    override def marshaller = InputScalaResultMarshaller
    override def unmarshal(marshaller: InputScalaResultMarshaller.type)(node: Option[marshaller.Node]) = node.get.asInstanceOf[T]
  }

  implicit def optionUnmarshaller[T, RM <: ResultMarshaller](implicit ity: InputTypeUnmarshaller[T, RM]): InputTypeUnmarshaller[Option[T], RM] =
    new OptionUnmarshaller[T, RM](ity)

  implicit def seqUnmarshaller[T, RM <: ResultMarshaller](implicit ity: InputTypeUnmarshaller[T, RM]): InputTypeUnmarshaller[Seq[T], RM] =
    new SeqUnmarshaller[T, RM](ity)

  implicit def enumUnmarshaller[T]: InputTypeUnmarshaller[EnumInput[T], InputScalaResultMarshaller.type] =
    new EnumUnmarshaller[T]

  implicit object StringUnmarshaller extends InputTypeUnmarshaller[String, InputScalaResultMarshaller.type] {
    type Result = String

    override def marshaller = InputScalaResultMarshaller
    override def unmarshal(marshaller: InputScalaResultMarshaller.type)(node: Option[marshaller.Node]) =
      marshaller.fromStringNode(node.get)
  }

  implicit object BooleanUnmarshaller extends InputTypeUnmarshaller[Boolean, InputScalaResultMarshaller.type] {
    type Result = Boolean

    override def marshaller = InputScalaResultMarshaller
    override def unmarshal(marshaller: InputScalaResultMarshaller.type)(node: Option[marshaller.Node]) =
      marshaller.fromBooleanNode(node.get)
  }

  implicit object IntUnmarshaller extends InputTypeUnmarshaller[Int, InputScalaResultMarshaller.type] {
    type Result = Int

    override def marshaller = InputScalaResultMarshaller
    override def unmarshal(marshaller: InputScalaResultMarshaller.type)(node: Option[marshaller.Node]) =
      marshaller.fromIntNode(node.get)
  }

  implicit object LongUnmarshaller extends InputTypeUnmarshaller[Long, InputScalaResultMarshaller.type] {
    type Result = Long

    override def marshaller = InputScalaResultMarshaller
    override def unmarshal(marshaller: InputScalaResultMarshaller.type)(node: Option[marshaller.Node]) =
      marshaller.fromBigIntNode(node.get).longValue()
  }

  implicit object DoubleUnmarshaller extends InputTypeUnmarshaller[Double, InputScalaResultMarshaller.type] {
    type Result = Double

    override def marshaller = InputScalaResultMarshaller
    override def unmarshal(marshaller: InputScalaResultMarshaller.type)(node: Option[marshaller.Node]) =
      marshaller.fromFloatNode(node.get)
  }

  implicit object BigDecimalUnmarshaller extends InputTypeUnmarshaller[BigDecimal, InputScalaResultMarshaller.type] {
    type Result = BigDecimal

    override def marshaller = InputScalaResultMarshaller
    override def unmarshal(marshaller: InputScalaResultMarshaller.type)(node: Option[marshaller.Node]) =
      marshaller.fromBigDecimalNode(node.get)
  }

  implicit object BigIntUnmarshaller extends InputTypeUnmarshaller[BigInt, InputScalaResultMarshaller.type] {
    type Result = BigInt

    override def marshaller = InputScalaResultMarshaller
    override def unmarshal(marshaller: InputScalaResultMarshaller.type)(node: Option[marshaller.Node]) =
      marshaller.fromBigIntNode(node.get)
  }

  class SimpleUnmarshaller[T](fn: InputScalaResultMarshaller.Node => T) extends InputTypeUnmarshaller[T, InputScalaResultMarshaller.type] {
    type Result = T

    override def marshaller = InputScalaResultMarshaller
    override def unmarshal(marshaller: InputScalaResultMarshaller.type)(node: Option[marshaller.Node]) =
      fn(node.get)
  }

  object InputScalaResultMarshaller extends ScalaResultMarshaller {
    override def isNode(obj: Any) = false

    override def arrayNode(values: Seq[Node], optional: Boolean) =
      if (optional)
        super.arrayNode(values) map (v => if (isNull(v)) None else Some(v))
      else
        super.arrayNode(values)

    override def addArrayNodeElem(array: Node, elem: Node, optional: Boolean) =
      super.addArrayNodeElem(array, if (optional) (if (isNull(elem)) None else Some(elem)) else elem)

    override def addMapNodeElemOpt(node: Node, key: String, value: Option[Node], optional: Boolean) =
      if (optional)
        super.addMapNodeElemOpt(node, key, value map (v => if (isNull(v)) None else Some(v)))
      else
        super.addMapNodeElemOpt(node, key, value flatMap (v => if (isNull(v)) None else Some(v)))

    override def addMapNodeElem(node: Node, key: String, value: Node, optional: Boolean) =
      if (optional)
        super.addMapNodeElem(node, key, if (isNull(value)) None else Some(value))
      else
        super.addMapNodeElem(node, key, value)
  }
}
