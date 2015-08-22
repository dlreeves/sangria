package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand._
import sangria.renderer.SchemaRenderer
import sangria.schema.{OptionInputType, LeafType}
import sangria.validation._

import scala.language.postfixOps

/**
 * Provided required arguments
 *
 * A field or directive is only valid if all required (non-null) field arguments
 * have been provided.
 */
class ProvidedNonNullArguments extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onLeave: ValidationVisit = {
      case ast.Field(_, name, args, _, _, pos) =>
        ctx.typeInfo.fieldDef match {
          case None => Right(Continue)
          case Some(fieldDef) =>
            val astArgs = args.map(_.name).toSet

            val errors = fieldDef.arguments.arguments.toVector.collect {
              case argDef if !astArgs.contains(argDef.name) && !argDef.argumentType.isInstanceOf[OptionInputType[_]] =>
                MissingFieldArgViolation(name, argDef.name, SchemaRenderer.renderTypeName(argDef.argumentType), ctx.sourceMapper, pos.toList)
            }

            if (errors.nonEmpty) Left(errors) else Right(Continue)
        }

      case ast.Directive(name, args, pos) =>
        ctx.typeInfo.directive match {
          case None => Right(Continue)
          case Some(dirDef) =>
            val astArgs = args.map(_.name).toSet

            val errors = dirDef.arguments.arguments.toVector.collect {
              case argDef if !astArgs.contains(argDef.name) && !argDef.argumentType.isInstanceOf[OptionInputType[_]] =>
                MissingFieldArgViolation(name, argDef.name, SchemaRenderer.renderTypeName(argDef.argumentType), ctx.sourceMapper, pos.toList)
            }

            if (errors.nonEmpty) Left(errors) else Right(Continue)
        }
    }
  }
}