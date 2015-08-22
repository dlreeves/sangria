package sangria.validation

import sangria.ast
import sangria.ast.{AstVisitorCommand, AstVisitor, FragmentDefinition}
import sangria.ast.AstVisitorCommand._
import sangria.schema._
import sangria.introspection.{SchemaMetaField, TypeMetaField, TypeNameMetaField}
import sangria.validation.rules._
import scala.collection.mutable.{Stack => MutableStack, Set => MutableSet, Map => MutableMap, ListBuffer}

trait QueryValidator {
  def validateQuery(schema: Schema[_, _], queryAst: ast.Document): List[Violation]
}

object QueryValidator {
  val allRules: List[ValidationRule] = List(
    new ArgumentsOfCorrectType,
    new DefaultValuesOfCorrectType,
    new FieldsOnCorrectType,
    new FragmentsOnCompositeType,
    new KnownArgumentNames,
    new KnownDirectives,
    new KnownFragmentNames,
    new KnownTypeNames,
    new LoneAnonymousOperation,
    new NoFragmentCycles,
    new NoUndefinedVariables,
    new NoUnusedFragments,
    new NoUnusedVariables,
    new OverlappingFieldsCanBeMerged,
    new PossibleFragmentSpreads,
    new ProvidedNonNullArguments,
    new ScalarLeafs,
    new UniqueArgumentNames,
    new UniqueFragmentNames,
    new UniqueOperationNames,
    new VariablesAreInputTypes,
    new VariablesInAllowedPosition
  )

  val empty = new QueryValidator {
    def validateQuery(schema: Schema[_, _], queryAst: ast.Document): List[Violation] = Nil
  }

  val default = new RuleBasedQueryValidator(allRules)
}

class RuleBasedQueryValidator(rules: List[ValidationRule]) extends QueryValidator {
  def validateQuery(schema: Schema[_, _], queryAst: ast.Document): List[Violation] = {
    val ctx = new ValidationContext(schema, queryAst, new TypeInfo(schema))

    validateUsingRules(queryAst, ctx, rules map (_ visitor ctx), true)

    ctx.violations
  }

  def validateUsingRules(queryAst: ast.AstNode, ctx: ValidationContext, visitors: List[ValidationRule#AstValidatingVisitor], topLevel: Boolean): Unit = AstVisitor.visitAst(
    doc = queryAst,
    onEnter = node => {
      ctx.typeInfo.enter(node)
      visitors foreach { visitor =>
        if (ctx.validVisitor(visitor)) {
          if (visitor.visitSpreadFragments && node.isInstanceOf[ast.FragmentDefinition] && topLevel)
            handleResult(ctx, node, visitor, Right(Skip))
          else if (visitor.onEnter.isDefinedAt(node))
            handleResult(ctx, node, visitor, visitor.onEnter(node))
        }
      }

      node match {
        case spread: ast.FragmentSpread if ctx.fragments contains spread.name =>
          val interested = visitors.filter(v => v.visitSpreadFragments && ctx.validVisitor(v))

          if (interested.nonEmpty)
            validateUsingRules(ctx.fragments(spread.name), ctx, interested, false)
        case _ => // do nothing
      }

      Continue
    },
    onLeave = node => {
      visitors foreach { visitor =>
        if (visitor.onLeave.isDefinedAt(node) && ctx.validVisitor(visitor)) {
          handleResult(ctx, node, visitor, visitor.onLeave(node))
        }

        if (ctx.skips.get(visitor).exists(_ eq node))
          ctx.skips.remove(visitor)
      }

      ctx.typeInfo.leave(node)
      Continue
    }
  )

  def handleResult(ctx: ValidationContext, node: ast.AstNode, visitor: ValidationRule#AstValidatingVisitor, visitRes: Either[Vector[Violation], AstVisitorCommand.Value]) =
    visitRes match {
      case Left(violation) =>
        ctx.addViolations(violation)
      case Right(Skip) =>
        ctx.skips(visitor) = node
      case Right(Break) =>
        ctx.ignoredVisitors += visitor
      case _ => // do nothing
    }
}

class ValidationContext(val schema: Schema[_, _], val doc: ast.Document, val typeInfo: TypeInfo) {
  // Using mutable data-structures and mutability to minimize validation footprint

  private val errors = ListBuffer[Violation]()

  val ignoredVisitors = MutableSet[ValidationRule#AstValidatingVisitor]()
  val skips = MutableMap[ValidationRule#AstValidatingVisitor, ast.AstNode]()

  lazy val fragments = doc.definitions
    .collect{case frDef: FragmentDefinition => frDef}
    .groupBy(_.name)
    .mapValues(_.head)

  def validVisitor(visitor: ValidationRule#AstValidatingVisitor) =
    !ignoredVisitors.contains(visitor) && !skips.contains(visitor)

  def sourceMapper = doc.sourceMapper

  def addViolation(v: Violation) = errors += v
  def addViolations(vs: Vector[Violation]) = errors ++= vs

  def violations = errors.toList
}

object ValidationContext {
  def isValidLiteralValue(tpe: InputType[_], value: ast.Value): Boolean = (tpe, value) match {
    case (_, _: ast.VariableValue) => true
    case (OptionInputType(ofType), v) =>
      isValidLiteralValue(ofType, v)
    case (ListInputType(ofType), ast.ListValue(values, _)) =>
      values.forall(isValidLiteralValue(ofType, _))
    case (ListInputType(ofType), v) =>
      isValidLiteralValue(ofType, v)
    case (io: InputObjectType[_], ast.ObjectValue(fields, _)) =>
      fields.forall(f => io.fieldsByName contains f.name) && {
        io.fields.forall { field =>
          val astField = fields.find(_.name == field.name)

          (astField, field.fieldType) match {
            case (None, _: OptionInputType[_]) => true
            case (None, _) => false
            case (Some(af), _) => isValidLiteralValue(field.fieldType, af.value)
          }
        }
      }
    case (io: InputObjectType[_], _) => false
    case (s: ScalarType[_], v) =>
      s.coerceInput(v).isRight
    case (enum: EnumType[_], v) =>
      enum.coerceInput(v).isRight

  }
}

class TypeInfo(schema: Schema[_, _]) {
  // Using mutable data-structures and mutability to minimize validation footprint

  private val typeStack: MutableStack[Option[Type]] = MutableStack()
  private val parentTypeStack: MutableStack[Option[CompositeType[_]]] = MutableStack()
  private val inputTypeStack: MutableStack[Option[InputType[_]]] = MutableStack()
  private val fieldDefStack: MutableStack[Option[Field[_, _]]] = MutableStack()
  private val ancestorStack: MutableStack[ast.AstNode] = MutableStack()

  var directive: Option[Directive[_]] = None
  var argument: Option[Argument[_]] = None

  def tpe = typeStack.headOption.flatten
  def previousParentType = parentTypeStack.drop(1).headOption.flatten
  def parentType = parentTypeStack.headOption.flatten
  def inputType = inputTypeStack.headOption.flatten
  def fieldDef = fieldDefStack.headOption.flatten
  def ancestors: Seq[ast.AstNode] = ancestorStack

  def enter(node: ast.AstNode) = {
    ancestorStack push node

    node match {
      case f: ast.Field =>
        val parent = parentType
        val fieldDef = parent flatMap (getFieldDef(_, f))
        val fieldType = fieldDef map (_.fieldType)

        fieldDefStack push fieldDef
        typeStack push fieldType

        pushParent()
      case ast.Directive(name, _, _) =>
        directive = schema.directivesByName get name
      case ast.OperationDefinition(ast.OperationType.Query, _, _, _, _, _) =>
        typeStack push Some(schema.query)
        pushParent()
      case ast.OperationDefinition(ast.OperationType.Mutation, _, _, _, _, _) =>
        typeStack push schema.mutation
        pushParent()
      case fd: ast.FragmentDefinition =>
        typeStack.push(schema.allTypes get fd.typeCondition.name)
        pushParent()
      case ifd: ast.InlineFragment =>
        typeStack.push(schema.allTypes get ifd.typeCondition.name)
        pushParent()
      case vd: ast.VariableDefinition =>
        inputTypeStack push schema.getInputType(vd.tpe)
      case a: ast.Argument =>
        argument = directive orElse fieldDef flatMap { withArgs =>
          withArgs.arguments.arguments find (_.name == a.name)
        }
        inputTypeStack push argument.map(_.inputValueType)
      case ast.ListValue(values, _) =>
        inputType match {
          case Some(it) => getNotNullType(it) match {
            case it: ListInputType[_] => inputTypeStack push Some(it.ofType)
            case _ => inputTypeStack push None
          }
          case None => inputTypeStack push None
        }
      case ast.ObjectField(name, value, _) =>
        val fieldType = inputType flatMap (it => getNamedType(it) match {
          case obj: InputObjectType[_] => obj.fieldsByName.get(name) map (_.inputValueType)
          case _ => None
        })

        inputTypeStack push fieldType
      case _ => // ignore
    }
  }

  def pushParent(): Unit = {
    tpe match {
      case Some(some) => getNamedType(some) match {
        case comp: CompositeType[_] => parentTypeStack push Some(comp)
        case _ => parentTypeStack push None
      }
      case _ => parentTypeStack push None
    }
  }

  def leave(node: ast.AstNode) = {
    node match {
      case f: ast.Field =>
        fieldDefStack.pop()
        typeStack.pop()
        parentTypeStack.pop()
      case ast.Directive(name, _, _) =>
        directive = None
      case ast.OperationDefinition(ast.OperationType.Query, _, _, _, _, _) =>
        typeStack.pop()
        parentTypeStack.pop()
      case ast.OperationDefinition(ast.OperationType.Mutation, _, _, _, _, _) =>
        typeStack.pop()
        parentTypeStack.pop()
      case fd: ast.FragmentDefinition =>
        typeStack.pop()
        parentTypeStack.pop()
      case fd: ast.InlineFragment =>
        typeStack.pop()
        parentTypeStack.pop()
      case vd: ast.VariableDefinition =>
        inputTypeStack.pop()
      case a: ast.Argument =>
        argument = None
        inputTypeStack.pop()
      case ast.ListValue(values, _) =>
        inputTypeStack.pop()
      case ast.ObjectField(name, value, _) =>
        inputTypeStack.pop()
      case _ => // ignore
    }

    ancestorStack.pop()
  }

  def getNamedType(it: Type): Type with Named = it match {
    case OptionInputType(ofType) => getNamedType(ofType)
    case OptionType(ofType) => getNamedType(ofType)
    case ListInputType(ofType) => getNamedType(ofType)
    case ListType(ofType) => getNamedType(ofType)
    case n: Named => n
    case t => throw new IllegalStateException("Expected named type, but got: " + t)
  }

  def getNotNullType(it: InputType[_]): InputType[_] = it match {
    case OptionInputType(ofType) => ofType
    case n => n
  }

  def getFieldDef(parent: CompositeType[_], astField: ast.Field): Option[Field[_, _]] = {
    if (astField.name == SchemaMetaField.name && schema.query.name == parent.name)
      Some(SchemaMetaField)
    else if (astField.name == TypeMetaField.name && schema.query.name == parent.name)
      Some(TypeMetaField)
    else if (astField.name == TypeNameMetaField.name)
      Some(TypeNameMetaField)
    else parent match {
      case o: ObjectLikeType[_, _] => o.getField(schema, astField.name)
      case _ => None
    }
  }
}