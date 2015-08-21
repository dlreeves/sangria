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
      marshaller.fromArrayNode(node.get).map(n => ity.unmarshal(marshaller)(Some(n)))
  }

  class EnumUnmarshaller[T] extends InputTypeUnmarshaller[EnumInput[T], ScalaResultMarshaller] {
    type Result = T

    override def marshaller = ResultMarshaller.defaultResultMarshaller
    override def unmarshal(marshaller: ScalaResultMarshaller)(node: Option[marshaller.Node]) = node.get.asInstanceOf[T]
  }

  implicit def optionUnmarshaller[T, RM <: ResultMarshaller](implicit ity: InputTypeUnmarshaller[T, RM]): InputTypeUnmarshaller[Option[T], RM] =
    new OptionUnmarshaller[T, RM](ity)

  implicit def seqUnmarshaller[T, RM <: ResultMarshaller](implicit ity: InputTypeUnmarshaller[T, RM]): InputTypeUnmarshaller[Seq[T], RM] =
    new SeqUnmarshaller[T, RM](ity)

  implicit def enumUnmarshaller[T]: InputTypeUnmarshaller[EnumInput[T], ScalaResultMarshaller] =
    new EnumUnmarshaller[T]

  implicit object StringUnmarshaller extends InputTypeUnmarshaller[String, ScalaResultMarshaller] {
    type Result = String

    override def marshaller = ResultMarshaller.defaultResultMarshaller
    override def unmarshal(marshaller: ScalaResultMarshaller)(node: Option[marshaller.Node]) =
      marshaller.fromStringNode(node.get)
  }

  implicit object BooleanUnmarshaller extends InputTypeUnmarshaller[Boolean, ScalaResultMarshaller] {
    type Result = Boolean

    override def marshaller = ResultMarshaller.defaultResultMarshaller
    override def unmarshal(marshaller: ScalaResultMarshaller)(node: Option[marshaller.Node]) =
      marshaller.fromBooleanNode(node.get)
  }

  implicit object IntUnmarshaller extends InputTypeUnmarshaller[Int, ScalaResultMarshaller] {
    type Result = Int

    override def marshaller = ResultMarshaller.defaultResultMarshaller
    override def unmarshal(marshaller: ScalaResultMarshaller)(node: Option[marshaller.Node]) =
      marshaller.fromIntNode(node.get)
  }

  implicit object LongUnmarshaller extends InputTypeUnmarshaller[Long, ScalaResultMarshaller] {
    type Result = Long

    override def marshaller = ResultMarshaller.defaultResultMarshaller
    override def unmarshal(marshaller: ScalaResultMarshaller)(node: Option[marshaller.Node]) =
      marshaller.fromBigIntNode(node.get).longValue()
  }

  implicit object DoubleUnmarshaller extends InputTypeUnmarshaller[Double, ScalaResultMarshaller] {
    type Result = Double

    override def marshaller = ResultMarshaller.defaultResultMarshaller
    override def unmarshal(marshaller: ScalaResultMarshaller)(node: Option[marshaller.Node]) =
      marshaller.fromFloatNode(node.get)
  }

  implicit object BigDecimalUnmarshaller extends InputTypeUnmarshaller[BigDecimal, ScalaResultMarshaller] {
    type Result = BigDecimal

    override def marshaller = ResultMarshaller.defaultResultMarshaller
    override def unmarshal(marshaller: ScalaResultMarshaller)(node: Option[marshaller.Node]) =
      marshaller.fromBigDecimalNode(node.get)
  }

  implicit object BigIntUnmarshaller extends InputTypeUnmarshaller[BigInt, ScalaResultMarshaller] {
    type Result = BigInt

    override def marshaller = ResultMarshaller.defaultResultMarshaller
    override def unmarshal(marshaller: ScalaResultMarshaller)(node: Option[marshaller.Node]) =
      marshaller.fromBigIntNode(node.get)
  }

  implicit object MapUnmarshaller extends InputTypeUnmarshaller[Map[String, Any], ScalaResultMarshaller] {
    type Result = Map[String, Any]

    override def marshaller = ResultMarshaller.defaultResultMarshaller
    override def unmarshal(marshaller: ScalaResultMarshaller)(node: Option[marshaller.Node]) =
      marshaller.fromMapNode(node.get) // todo
  }
}
