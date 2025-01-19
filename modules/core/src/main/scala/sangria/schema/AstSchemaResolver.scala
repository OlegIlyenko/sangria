package sangria.schema

import sangria.ast
import sangria.marshalling.{InputUnmarshaller, ResultMarshallerForType}
import sangria.schema.AstSchemaBuilder.TypeName

sealed trait AstSchemaResolver[Ctx]

case class AdditionalTypes[Ctx](additionalTypes: List[MaterializedType])
    extends AstSchemaResolver[Ctx]

object AdditionalTypes {
  def apply[Ctx](
      schema: Schema[Ctx, _],
      additionalTypes: (Type with Named)*): AdditionalTypes[Ctx] = {
    val origin = ExistingSchemaOrigin(schema)

    AdditionalTypes(additionalTypes.toList.map(MaterializedType(origin, _)))
  }

  def apply[Ctx](additionalTypes: (Type with Named)*): AdditionalTypes[Ctx] =
    AdditionalTypes(additionalTypes.toList.map(MaterializedType(StandaloneOrigin, _)))
}

case class AdditionalDirectives[Ctx](additionalDirectives: Seq[Directive])
    extends AstSchemaResolver[Ctx]

case class DirectiveResolver[Ctx](
    directive: Directive,
    resolve: AstDirectiveContext[Ctx] => Action[Ctx, Any],
    complexity: Option[ComplexityDirectiveContext[Ctx] => (Ctx, Args, Double) => Double] = None)
    extends AstSchemaResolver[Ctx]

case class DirectiveFieldProvider[Ctx](
    directive: Directive,
    resolve: DirectiveFieldProviderContext[Ctx] => List[MaterializedField[Ctx, _]])
    extends AstSchemaResolver[Ctx]

case class DynamicDirectiveFieldProvider[Ctx, A](
    directiveName: String,
    resolve: DynamicDirectiveFieldProviderContext[Ctx, A] => List[MaterializedField[Ctx, _]])(
    implicit val marshaller: ResultMarshallerForType[A])
    extends AstSchemaResolver[Ctx]

case class DirectiveInputTypeResolver[Ctx](
    directive: Directive,
    resolve: AstDirectiveInputTypeContext[Ctx] => InputType[Any])
    extends AstSchemaResolver[Ctx]

case class DirectiveOutputTypeResolver[Ctx](
    directive: Directive,
    resolve: AstDirectiveOutputTypeContext[Ctx] => OutputType[Any])
    extends AstSchemaResolver[Ctx]

case class InputTypeResolver[Ctx](
    resolve: PartialFunction[AstInputTypeContext[Ctx], InputType[Any]])
    extends AstSchemaResolver[Ctx]

case class OutputTypeResolver[Ctx](
    resolve: PartialFunction[AstOutputTypeContext[Ctx], OutputType[Any]])
    extends AstSchemaResolver[Ctx]

case class DirectiveScalarResolver[Ctx](
    directive: Directive,
    resolve: AstDirectiveScalarContext => ScalarType[_])
    extends AstSchemaResolver[Ctx]

case class SimpleEnumValueResolver[Ctx](
    resolve: PartialFunction[
      (Either[ast.EnumTypeDefinition, EnumType[_]], ast.EnumValueDefinition),
      String])
    extends AstSchemaResolver[Ctx]

case class ExistingScalarResolver[Ctx](
    resolve: PartialFunction[ExistingScalarContext[Ctx], ScalarType[Any]])
    extends AstSchemaResolver[Ctx]

case class ExistingEnumResolver[Ctx](
    resolve: PartialFunction[ExistingEnumContext[Ctx], EnumType[Any]])
    extends AstSchemaResolver[Ctx]

case class ScalarResolver[Ctx](resolve: PartialFunction[ast.ScalarTypeDefinition, ScalarType[_]])
    extends AstSchemaResolver[Ctx]

case class DynamicDirectiveResolver[Ctx, T](
    directiveName: String,
    resolve: DynamicDirectiveContext[Ctx, T] => Action[Ctx, Any],
    complexity: Option[ComplexityDynamicDirectiveContext[Ctx, T] => (Ctx, Args, Double) => Double] =
      None)(implicit val marshaller: ResultMarshallerForType[T])
    extends AstSchemaResolver[Ctx]

case class FieldResolver[Ctx](
    resolve: PartialFunction[
      (Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]], ast.FieldDefinition),
      Context[Ctx, _] => Action[Ctx, Any]],
    complexity: PartialFunction[
      (Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]], ast.FieldDefinition),
      (Ctx, Args, Double) => Double] = PartialFunction.empty)
    extends AstSchemaResolver[Ctx]

object FieldResolver {
  def map[Ctx](
      config: (String, Map[String, Context[Ctx, _] => Action[Ctx, Any]])*): FieldResolver[Ctx] = {
    val configMap = config.toMap

    FieldResolver[Ctx](
      {
        case (TypeName(name), field)
            if configMap.contains(name) && configMap(name).contains(field.name) =>
          configMap(name)(field.name)
      },
      PartialFunction.empty)
  }

  def defaultInput[Ctx, In: InputUnmarshaller] =
    ResolverBasedAstSchemaBuilder.defaultInputResolver[Ctx, In]
}

case class ExistingFieldResolver[Ctx](
    resolve: PartialFunction[
      (MatOrigin, Option[ObjectLikeType[Ctx, _]], Field[Ctx, _]),
      Context[Ctx, _] => Action[Ctx, Any]])
    extends AstSchemaResolver[Ctx]

object ExistingFieldResolver {
  def map[Ctx](config: (String, Map[String, Context[Ctx, _] => Action[Ctx, Any]])*)
      : ExistingFieldResolver[Ctx] = {
    val configMap = config.toMap

    ExistingFieldResolver {
      case (_, tpe, field)
          if tpe.isDefined && configMap.contains(tpe.get.name) && configMap(tpe.get.name).contains(
            field.name) =>
        configMap(tpe.get.name)(field.name)
    }
  }

  def defaultInput[Ctx, In: InputUnmarshaller] =
    ResolverBasedAstSchemaBuilder.defaultExistingInputResolver[Ctx, In]
}

case class AnyFieldResolver[Ctx](
    resolve: PartialFunction[MatOrigin, Context[Ctx, _] => Action[Ctx, Any]])
    extends AstSchemaResolver[Ctx]

object AnyFieldResolver {
  def defaultInput[Ctx, In: InputUnmarshaller] =
    ResolverBasedAstSchemaBuilder.defaultAnyInputResolver[Ctx, In]
}

case class InstanceCheck[Ctx](fn: InstanceCheckContext[Ctx] => (Any, Class[_]) => Boolean)
    extends AstSchemaResolver[Ctx]

object InstanceCheck {
  def simple[Ctx](fn: Any => String): InstanceCheck[Ctx] =
    InstanceCheck(c => (value, _) => fn(value) == c.definition.name)

  def field[Ctx, T: InputUnmarshaller]: InstanceCheck[Ctx] =
    field[Ctx, T]("type")

  def field[Ctx, T: InputUnmarshaller](fieldName: String): InstanceCheck[Ctx] = {
    val iu = implicitly[InputUnmarshaller[T]]

    InstanceCheck { c => (value, _) =>
      val node = value.asInstanceOf[T]

      if (!iu.isMapNode(node)) false
      else
        iu.getMapValue(node, fieldName) match {
          case Some(v) => iu.isScalarNode(v) && iu.getScalaScalarValue(v) == c.definition.name
          case None => false
        }
    }
  }
}

case class ExistingInstanceCheck[Ctx](
    fn: ExistingInstanceCheckContext[Ctx] => (Any, Class[_]) => Boolean)
    extends AstSchemaResolver[Ctx]

case class ConflictResolver[Ctx](resolve: (MatOrigin, Vector[MaterializedType]) => MaterializedType)
    extends AstSchemaResolver[Ctx]

sealed trait AstSchemaGenericResolver[T] {
  def locations: Set[DirectiveLocation.Value]
  def directiveName: String
}

case class GenericDirectiveResolver[T](
    directive: Directive,
    locations: Set[DirectiveLocation.Value] = Set.empty,
    resolve: GenericDirectiveContext => Option[T])
    extends AstSchemaGenericResolver[T] {
  def directiveName = directive.name
}

trait WithTypeLookup[Ctx] {
  def origin: MatOrigin
  def materializer: AstSchemaMaterializer[Ctx]

  def objectType(typeName: String): ObjectType[Ctx, Any] =
    materializer.getObjectType(origin, ast.NamedType(typeName))

  def scalarType(typeName: String): ScalarType[Any] =
    materializer.getScalarType(origin, ast.NamedType(typeName))

  def interfaceType(typeName: String): InterfaceType[Ctx, Any] =
    materializer.getInterfaceType(origin, ast.NamedType(typeName))

  def inputType(typeName: String): InputType[_] =
    materializer.getInputType(origin, ast.NamedType(typeName), optional = false)

  def inputType(tpe: ast.Type, replacementNamedType: InputType[_]): InputType[_] =
    materializer.getInputType(origin, tpe, Some(replacementNamedType))

  def outputType(typeName: String): OutputType[_] =
    materializer.getOutputType(origin, ast.NamedType(typeName), optional = false)

  def outputType(tpe: ast.Type, replacementNamedType: OutputType[_]): OutputType[_] =
    materializer.getOutputType(origin, tpe, Some(replacementNamedType))
}

case class GenericDynamicDirectiveResolver[T, A](
    directiveName: String,
    locations: Set[DirectiveLocation.Value] = Set.empty,
    resolve: GenericDynamicDirectiveContext[A] => Option[T])(implicit
    val marshaller: ResultMarshallerForType[A])
    extends AstSchemaGenericResolver[T]

case class AstDirectiveInputTypeContext[Ctx](
    origin: MatOrigin,
    directive: ast.Directive,
    schemaDefinition: Option[Type with Named],
    astDefinition: Option[ast.TypeSystemDefinition],
    astField: Option[ast.FieldDefinition],
    definition: ast.InputValueDefinition,
    materializer: AstSchemaMaterializer[Ctx],
    args: Args)
    extends WithArguments
    with WithTypeLookup[Ctx]

case class AstDirectiveOutputTypeContext[Ctx](
    origin: MatOrigin,
    directive: ast.Directive,
    typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
    fieldDefinition: ast.FieldDefinition,
    extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
    materializer: AstSchemaMaterializer[Ctx],
    args: Args)
    extends WithArguments
    with WithTypeLookup[Ctx]

case class AstInputTypeContext[Ctx](
    origin: MatOrigin,
    schemaDefinition: Option[Type with Named],
    astDefinition: Option[ast.TypeSystemDefinition],
    astField: Option[ast.FieldDefinition],
    definition: ast.InputValueDefinition,
    materializer: AstSchemaMaterializer[Ctx])
    extends WithTypeLookup[Ctx]

case class AstOutputTypeContext[Ctx](
    origin: MatOrigin,
    typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
    fieldDefinition: ast.FieldDefinition,
    extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
    materializer: AstSchemaMaterializer[Ctx])
    extends WithTypeLookup[Ctx]

case class AstDirectiveContext[Ctx](
    directive: ast.Directive,
    typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
    fieldDefinition: ast.FieldDefinition,
    extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
    ctx: Context[Ctx, _],
    lastValue: Option[Action[Ctx, Any]],
    args: Args)
    extends WithArguments

case class DirectiveFieldProviderContext[Ctx](
    origin: MatOrigin,
    directive: ast.Directive,
    typeDefinition: ast.TypeDefinition,
    extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
    materializer: AstSchemaMaterializer[Ctx],
    args: Args)
    extends WithArguments
    with WithTypeLookup[Ctx]

case class DynamicDirectiveFieldProviderContext[Ctx, A](
    origin: MatOrigin,
    directive: ast.Directive,
    typeDefinition: ast.TypeDefinition,
    extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
    materializer: AstSchemaMaterializer[Ctx],
    args: A)
    extends WithTypeLookup[Ctx]

case class GenericDirectiveContext(directive: ast.Directive, astNode: ast.AstNode, args: Args)
    extends WithArguments

case class GenericDynamicDirectiveContext[A](
    directive: ast.Directive,
    astNode: ast.AstNode,
    args: A)

case class AstDirectiveScalarContext(
    directive: ast.Directive,
    definition: ast.ScalarTypeDefinition,
    args: Args)
    extends WithArguments

case class ExistingScalarContext[Ctx](
    origin: MatOrigin,
    extensions: Vector[ast.ScalarTypeExtensionDefinition],
    existing: ScalarType[Any],
    materializer: AstSchemaMaterializer[Ctx])
    extends WithTypeLookup[Ctx]

case class ExistingEnumContext[Ctx](
    origin: MatOrigin,
    extensions: Vector[ast.EnumTypeExtensionDefinition],
    existing: EnumType[Any],
    materializer: AstSchemaMaterializer[Ctx])
    extends WithTypeLookup[Ctx]

case class DynamicDirectiveContext[Ctx, In](
    directive: ast.Directive,
    typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
    fieldDefinition: ast.FieldDefinition,
    extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
    ctx: Context[Ctx, _],
    lastValue: Option[Action[Ctx, Any]],
    args: In)

case class ComplexityDynamicDirectiveContext[Ctx, In](
    directive: ast.Directive,
    typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
    fieldDefinition: ast.FieldDefinition,
    args: In)

case class ComplexityDirectiveContext[Ctx](
    directive: ast.Directive,
    typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
    fieldDefinition: ast.FieldDefinition,
    args: Args)

case class InstanceCheckContext[Ctx](
    origin: MatOrigin,
    definition: ast.ObjectTypeDefinition,
    extensions: List[ast.ObjectTypeExtensionDefinition])

case class ExistingInstanceCheckContext[Ctx](
    origin: MatOrigin,
    tpe: ObjectType[Ctx, _],
    extensions: List[ast.ObjectTypeExtensionDefinition])
