package sangria

import sangria.schema.InputTypeUnmarshaller.InputScalaResultMarshaller
import sangria.validation._

package object schema {
  val IntType = ScalarType[Int]("Int",
    description = Some("32-bit integer value"),
    coerceOutput = ast.IntValue(_),
    coerceUserInput = {
      case i: Int => Right(i)
      case i: Long if i.isValidInt => Right(i.toInt)
      case i: BigInt if !i.isValidInt => Left(BigIntCoercionViolation)
      case i: BigInt => Right(i.intValue)
      case _ => Left(IntCoercionViolation)
    },
    coerceInput = {
      case ast.IntValue(i, _) => Right(i)
      case ast.BigIntValue(i, _) if !i.isValidInt => Left(BigIntCoercionViolation)
      case ast.BigIntValue(i, _) => Right(i.intValue)
      case _ => Left(IntCoercionViolation)
    })

  val LongType = ScalarType[Long]("Long",
    description = Some("64-bit integer value"),
    coerceOutput = l => ast.BigIntValue(BigInt(l)),
    coerceUserInput = {
      case i: Int => Right(i: Long)
      case i: Long => Right(i)
      case i: BigInt if !i.isValidLong => Left(BigLongCoercionViolation)
      case i: BigInt => Right(i.longValue)
      case _ => Left(LongCoercionViolation)
    },
    coerceInput = {
      case ast.IntValue(i, _) => Right(i: Long)
      case ast.BigIntValue(i, _) if !i.isValidLong => Left(BigLongCoercionViolation)
      case ast.BigIntValue(i, _) => Right(i.longValue)
      case _ => Left(LongCoercionViolation)
    })

  val BigIntType = ScalarType[BigInt]("BigInt",
    description = Some("Arbitrary big integer value"),
    coerceOutput = ast.BigIntValue(_),
    coerceUserInput = {
      case i: Int => Right(BigInt(i))
      case i: Long => Right(BigInt(i))
      case i: BigInt => Right(i)
      case _ => Left(IntCoercionViolation)
    },
    coerceInput = {
      case ast.IntValue(i, _) => Right(i)
      case ast.BigIntValue(i, _) => Right(i)
      case _ => Left(IntCoercionViolation)
    })

  val FloatType = ScalarType[Double]("Float",
    coerceOutput = ast.FloatValue(_),
    coerceUserInput = {
      case i: Int => Right(i.toDouble)
      case i: Long => Right(i.toDouble)
      case i: BigInt if !i.isValidDouble => Left(BigDecimalCoercionViolation)
      case i: BigInt => Right(i.doubleValue())
      case d: Double => Right(d)
      case d: BigDecimal if !d.isDecimalDouble => Left(BigDecimalCoercionViolation)
      case d: BigDecimal => Right(d.doubleValue())
      case _ => Left(FloatCoercionViolation)
    },
    coerceInput = {
      case ast.FloatValue(d, _) => Right(d)
      case ast.BigDecimalValue(d, _) if !d.isDecimalDouble => Left(BigDecimalCoercionViolation)
      case ast.BigDecimalValue(d, _) => Right(d.doubleValue)
      case ast.IntValue(i, _) => Right(i)
      case ast.BigIntValue(i, _) if !i.isValidDouble => Left(BigDecimalCoercionViolation)
      case ast.BigIntValue(i, _) => Right(i.doubleValue)
      case _ => Left(FloatCoercionViolation)
    })

  val BigDecimalType = ScalarType[BigDecimal]("BigDecimal",
    coerceOutput = ast.BigDecimalValue(_),
    coerceUserInput = {
      case i: Int => Right(BigDecimal(i))
      case i: Long => Right(BigDecimal(i))
      case i: BigInt => Right(BigDecimal(i))
      case d: Double => Right(BigDecimal(d))
      case d: BigDecimal => Right(d)
      case _ => Left(FloatCoercionViolation)
    },
    coerceInput = {
      case ast.BigDecimalValue(d, _) => Right(d)
      case ast.FloatValue(d, _) => Right(BigDecimal(d))
      case ast.IntValue(i, _) => Right(BigDecimal(i))
      case ast.BigIntValue(i, _) => Right(BigDecimal(i))
      case _ => Left(FloatCoercionViolation)
    })

  val BooleanType = ScalarType[Boolean]("Boolean",
    coerceOutput = b => ast.BooleanValue(b),
    coerceUserInput = {
      case b: Boolean => Right(b)
      case _ => Left(BooleanCoercionViolation)
    },
    coerceInput = {
      case ast.BooleanValue(b, _) => Right(b)
      case _ => Left(BooleanCoercionViolation)
    })

  val StringType = ScalarType[String]("String",
    coerceOutput = s => ast.StringValue(s),
    coerceUserInput = {
      case s: String => Right(s)
      case _ => Left(StringCoercionViolation)
    },
    coerceInput = {
      case ast.StringValue(s, _) => Right(s)
      case _ => Left(StringCoercionViolation)
    })

  val IDType = ScalarType[String]("ID",
    coerceOutput = s => ast.StringValue(s),
    coerceUserInput = {
      case s: String => Right(s)
      case i: Int => Right(i.toString)
      case i: Long => Right(i.toString)
      case i: BigInt => Right(i.toString)
      case _ => Left(IDCoercionViolation)
    },
    coerceInput = {
      case ast.StringValue(id, _) => Right(id)
      case ast.IntValue(id, _) => Right(id.toString)
      case ast.BigIntValue(id, _) => Right(id.toString)
      case _ => Left(IDCoercionViolation)
    })

  val BuiltinScalars = IntType :: LongType :: BigIntType :: FloatType :: BigDecimalType :: BooleanType :: StringType :: IDType :: Nil

  val IfArg = Argument("if", BooleanType, "Included when true.")

  case class Inclusion(ifCond: Boolean)

  object Inclusion {
    implicit object InclusionUnmarshaller extends InputTypeUnmarshaller[Inclusion, InputScalaResultMarshaller.type] {
      type Result = Inclusion

      override def marshaller = InputScalaResultMarshaller
      override def unmarshal(marshaller: InputScalaResultMarshaller.type)(node: Option[marshaller.Node]) =
        Inclusion(Args(marshaller.fromMapNode(node.get)).arg(IfArg))
    }
  }

  val IncludeDirective = Directive[Inclusion]("include",
    description = Some("Directs the executor to include this field or fragment only when the `if` argument is true."),
    arguments = IfArg :: Nil,
    onOperation = false,
    onFragment = true,
    onField = true,
    shouldInclude = ctx => ctx.args.ifCond)

  val SkipDirective = Directive[Inclusion]("skip",
    description = Some("Directs the executor to skip this field or fragment when the `if` argument is true."),
    arguments = IfArg :: Nil,
    onOperation = false,
    onFragment = true,
    onField = true,
    shouldInclude = ctx => !ctx.args.ifCond)

  val BuiltinDirectives = IncludeDirective :: SkipDirective :: Nil

  def fields[Ctx, Val](fields: Field[Ctx, Val]*): List[Field[Ctx, Val]] = fields.toList

  def interfaces[Ctx, Concrete](interfaces: PossibleInterface[Ctx, Concrete]*): List[PossibleInterface[Ctx, Concrete]] =
    interfaces.toList

  def possibleTypes[Ctx, Abstract](objectTypes: PossibleObject[Ctx, Abstract]*): List[PossibleObject[Ctx, Abstract]] =
    objectTypes.toList
}
