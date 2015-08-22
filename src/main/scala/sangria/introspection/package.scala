package sangria

import sangria.parser.QueryParser
import sangria.schema._

import scala.util.Success

package object introspection {
  object TypeKind extends Enumeration {
    val Scalar, Object, Interface, Union, Enum, InputObject, List, NonNull = Value
  }

  val __TypeKind = EnumType("__TypeKind", Some("An enum describing what kind of type a given __Type is."), List(
    EnumValue("SCALAR", value = TypeKind.Scalar, description = Some("Indicates this type is a scalar.")),
    EnumValue("OBJECT", value = TypeKind.Object, description = Some(
      "Indicates this type is an object. " +
      "`fields` and `interfaces` are valid fields.")),
    EnumValue("INTERFACE", value = TypeKind.Interface, description = Some(
      "Indicates this type is an interface. " +
      "`fields` and `possibleTypes` are valid fields.")),
    EnumValue("UNION", value = TypeKind.Union, description = Some(
      "Indicates this type is a union. " +
      "`possibleTypes` is a valid field.")),
    EnumValue("ENUM", value = TypeKind.Enum, description = Some(
      "Indicates this type is an enum. " +
      "`enumValues` is a valid field.")),
    EnumValue("INPUT_OBJECT", value = TypeKind.InputObject, description = Some(
      "Indicates this type is an input object. " +
      "`inputFields` is a valid field.")),
    EnumValue("LIST", value = TypeKind.List, description = Some(
      "Indicates this type is a list. " +
      "`ofType` is a valid field.")),
    EnumValue("NON_NULL", value = TypeKind.NonNull, description = Some(
      "Indicates this type is a non-null. " +
      "`ofType` is a valid field."))
  ))

  val __Field = ObjectType("__Field", () => fields[Unit, Field[_, _]](
    Field("name", StringType)(_.value.name),
    Field("description", OptionType(StringType))(_.value.description),
    Field("args", ListType(__InputValue))(_.value.arguments.arguments),
    Field("type", __Type)(false -> _.value.fieldType),
    Field("isDeprecated", BooleanType)(_.value.deprecationReason.isDefined),
    Field("deprecationReason", OptionType(StringType))(_.value.deprecationReason)
  ))

  val includeDeprecated = Argument("includeDeprecated", OptionInputType(BooleanType), false)

  private def getKind(value: (Boolean, Type)) = {
    def identifyKind(t: Type, optional: Boolean): TypeKind.Value = t match {
      case OptionType(ofType) => identifyKind(ofType, true)
      case OptionInputType(ofType) => identifyKind(ofType, true)
      case _ if !optional => TypeKind.NonNull
      case _: ScalarType[_] => TypeKind.Scalar
      case _: ObjectType[_, _] => TypeKind.Object
      case _: InterfaceType[_, _] => TypeKind.Interface
      case _: UnionType[_] => TypeKind.Union
      case _: EnumType[_] => TypeKind.Enum
      case _: InputObjectType[_] => TypeKind.InputObject
      case _: ListType[_] | _: ListInputType[_] => TypeKind.List
    }

    val (fromTypeList, tpe) = value

    identifyKind(tpe, fromTypeList)
  }

  private def findNamed(tpe: Type): Option[Type with Named] = tpe match {
    case o: OptionType[_] => findNamed(o.ofType)
    case o: OptionInputType[_] => findNamed(o.ofType)
    case l: ListType[_] => findNamed(l.ofType)
    case l: ListInputType[_] => findNamed(l.ofType)
    case n: Type with Named => Some(n)
    case _ => None
  }

  private def findListType(tpe: Type): Option[Type] = tpe match {
    case o: OptionType[_] => findListType(o.ofType)
    case o: OptionInputType[_] => findListType(o.ofType)
    case l: ListType[_] => Some(l.ofType)
    case l: ListInputType[_] => Some(l.ofType)
    case _ => None
  }

  val __Type: ObjectType[Unit, (Boolean, Type)] = ObjectType("__Type", () => List[Field[Unit, (Boolean, Type)]](
    Field("kind", __TypeKind)(ctx => getKind(ctx.value)),
    Field("name", OptionType(StringType))(ctx => getKind(ctx.value) match {
      case TypeKind.NonNull | TypeKind.List => None
      case _ => findNamed(ctx.value._2) map (_.name)
    }),
    Field("description", OptionType(StringType))(ctx => getKind(ctx.value) match {
      case TypeKind.NonNull | TypeKind.List => None
      case _ => findNamed(ctx.value._2) flatMap (_.description)
    }),
    Field("fields", OptionType(ListType(__Field)), arguments = includeDeprecated :: Nil) { ctx =>
      val incDep: Boolean = ctx.args.arg(includeDeprecated)
      val (_, tpe) = ctx.value

      tpe match {
        case t: ObjectLikeType[_, _] if incDep => Some(t.fields.asInstanceOf[List[Field[_, _]]])
        case t: ObjectLikeType[_, _] => Some(t.fields.asInstanceOf[List[Field[_, _]]].filter(_.deprecationReason.isEmpty))
        case _ => None
      }
    },
    Field("interfaces", OptionType(ListType(__Type)))(_.value._2 match {
      case t: ObjectType[_, _] => Some(t.interfaces.asInstanceOf[List[Type]] map (true -> _))
      case _ => None
    }),
    Field("possibleTypes", OptionType(ListType(__Type)))(ctx => ctx.value._2 match {
      case t: AbstractType => ctx.schema.possibleTypes.get(t.name) map { tpe =>
        t match {
          case _: UnionType[_] => tpe map (true -> _)
          case _ => tpe sortBy (_.name) map (true -> _)
        }
      }
      case _ => None
    }),
    Field("enumValues", OptionType(ListType(__EnumValue)), arguments = includeDeprecated :: Nil) { ctx =>
      val incDep = ctx.args.arg(includeDeprecated)

      ctx.value._2 match {
        case enum: EnumType[_] if incDep => Some(enum.values)
        case enum: EnumType[_] => Some(enum.values.filter(_.deprecationReason.isEmpty))
        case _ => None
      }
    },
    Field("inputFields", OptionType(ListType(__InputValue)))(_.value._2 match {
      case io: InputObjectType[_] => Some(io.fields)
      case _ => None
    }),
    Field("ofType", OptionType(__Type))(ctx => getKind(ctx.value) match {
      case TypeKind.NonNull => Some(true -> ctx.value._2)
      case TypeKind.List => findListType(ctx.value._2) map (false -> _)
      case _ => None
    })
  ))

  val __InputValue: ObjectType[Unit, InputValue[_]] = ObjectType(
    name = "__InputValue",
    fields = List[Field[Unit, InputValue[_]]](
      Field("name", StringType)(_.value.name),
      Field("description", OptionType(StringType))(_.value.description),
      Field("type", __Type)(false -> _.value.inputValueType),
      Field("defaultValue", OptionType(StringType))(
        ctx => ctx.value.defaultValue.map(ctx.renderInputValueCompact(_, ctx.value.inputValueType)))
    ))

  val __EnumValue: ObjectType[Unit, EnumValue[_]] = ObjectType(
    name = "__EnumValue",
    fields = List[Field[Unit, EnumValue[_]]](
      Field("name", StringType)(_.value.name),
      Field("description", OptionType(StringType))(_.value.description),
      Field("isDeprecated", BooleanType)(_.value.deprecationReason.isDefined),
      Field("deprecationReason", OptionType(StringType))(_.value.deprecationReason)
    ))

  val __Directive = ObjectType(
    name = "__Directive",
    fields = fields[Unit, Directive[_]](
      Field("name", StringType)(_.value.name),
      Field("description", OptionType(StringType))(_.value.description),
      Field("args", ListType(__InputValue))(_.value.arguments.arguments),
      Field("onOperation", BooleanType)(_.value.onOperation),
      Field("onFragment", BooleanType)(_.value.onFragment),
      Field("onField", BooleanType)(_.value.onField)
    ))


  val __Schema = ObjectType(
    name = "__Schema",
    description =
        "A GraphQL Schema defines the capabilities of a GraphQL " +
        "server. It exposes all available types and directives on " +
        "the server, as well as the entry points for query and " +
        "mutation operations.",
    fields = List[Field[Unit, Schema[Any, Any]]](
      Field("types", ListType(__Type), Some("A list of all types supported by this server."))(_.value.typeList map (true -> _)),
      Field("queryType", __Type, Some("The type that query operations will be rooted at."))(true -> _.value.query),
      Field("mutationType", OptionType(__Type),
        Some("If this server supports mutation, the type that mutation operations will be rooted at."))(_.value.mutation map (true -> _)),
      Field("directives", ListType(__Directive),
        Some("A list of all directives supported by this server."))(_.value.directives)))

  val SchemaMetaField: Field[Unit, Unit] = Field(
    name = "__schema",
    fieldType = __Schema,
    description = Some("Access the current type schema of this server.")
  )(_.schema.asInstanceOf[Schema[Any, Any]])

  val TypeMetaField: Field[Unit, Unit] = Field(
    name = "__type",
    fieldType = OptionType(__Type),
    description = Some("Request the type information of a single type."),
    arguments = Argument("name", StringType) :: Nil)(
    resolve = ctx => ctx.schema.types get ctx.args.arg[String]("name") map (true -> _._2))

  val TypeNameMetaField: Field[Unit, Unit] = Field(
    name = "__typename",
    fieldType = StringType,
    description = Some("The name of the current Object type at runtime."))(
    resolve = ctx => ctx.parentType.name)

  lazy val Success(introspectionQuery) = QueryParser.parse(
    """
      |query IntrospectionQuery {
      |  __schema {
      |    queryType { name }
      |    mutationType { name }
      |    types {
      |      ...FullType
      |    }
      |    directives {
      |      name
      |      args {
      |        name
      |        type { ...TypeRef }
      |        defaultValue
      |      }
      |      onOperation
      |      onFragment
      |      onField
      |    }
      |  }
      |}
      |
      |fragment FullType on __Type {
      |  kind
      |  name
      |  fields {
      |    name
      |    args {
      |      name
      |      type { ...TypeRef }
      |      defaultValue
      |    }
      |    type {
      |      ...TypeRef
      |    }
      |    isDeprecated
      |    deprecationReason
      |  }
      |  inputFields {
      |    name
      |    type { ...TypeRef }
      |    defaultValue
      |  }
      |  interfaces {
      |    ...TypeRef
      |  }
      |  enumValues {
      |    name
      |    isDeprecated
      |    deprecationReason
      |  }
      |  possibleTypes {
      |    ...TypeRef
      |  }
      |}
      |
      |fragment TypeRef on __Type {
      |  kind
      |  name
      |  ofType {
      |    kind
      |    name
      |    ofType {
      |      kind
      |      name
      |      ofType {
      |        kind
      |        name
      |      }
      |    }
      |  }
      |}
    """.stripMargin)
}
