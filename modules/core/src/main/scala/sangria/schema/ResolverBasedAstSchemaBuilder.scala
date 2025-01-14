package sangria.schema

import language.existentials
import sangria.ast
import sangria.execution.MaterializedSchemaValidationError
import sangria.marshalling.{InputUnmarshaller, ResultMarshallerForType, ToInput}
import sangria.renderer.SchemaRenderer
import sangria.validation.{QueryValidator, UnknownDirectiveViolation, ValidatorStack, Violation}
import sangria.validation.rules.{ExecutableDefinitions, KnownDirectives}
import sangria.visitor.VisitorCommand

import scala.collection.immutable.VectorBuilder
import scala.util.control.NonFatal

class ResolverBasedAstSchemaBuilder[Ctx](val resolvers: Seq[AstSchemaResolver[Ctx]])
    extends DefaultAstSchemaBuilder[Ctx] {
  protected lazy val directiveResolvers = resolvers.collect { case dr: DirectiveResolver[Ctx] =>
    dr
  }
  protected lazy val directiveScalarResolvers = resolvers.collect {
    case dr: DirectiveScalarResolver[Ctx] => dr
  }
  protected lazy val directiveInpResolvers = resolvers.collect {
    case dr: DirectiveInputTypeResolver[Ctx] => dr
  }
  protected lazy val directiveOutResolvers = resolvers.collect {
    case dr: DirectiveOutputTypeResolver[Ctx] => dr
  }
  protected lazy val directiveProviderDirs = resolvers.collect {
    case dr: DirectiveFieldProvider[Ctx] => dr.directive
  }
  protected lazy val directiveDynProviderDirNames = resolvers.collect {
    case dr: DynamicDirectiveFieldProvider[Ctx, _] => dr.directiveName
  }
  protected lazy val additionalDirectives = resolvers.flatMap {
    case AdditionalDirectives(ad) => ad
    case _ => Nil
  }

  protected lazy val dynamicDirectiveNames =
    resolvers.collect { case dr: DynamicDirectiveResolver[Ctx, _] => dr.directiveName }.toSet ++
      directiveDynProviderDirNames

  protected lazy val directives =
    directiveResolvers.map(_.directive) ++
      directiveScalarResolvers.map(_.directive) ++
      directiveInpResolvers.map(_.directive) ++
      directiveOutResolvers.map(_.directive) ++
      additionalDirectives ++
      directiveProviderDirs

  protected lazy val stubQueryType =
    ObjectType("Query", fields[Unit, Unit](Field("stub", StringType, resolve = _ => "stub")))
  protected lazy val validationSchema =
    Schema(stubQueryType, directives = directives.toList ++ BuiltinDirectives)

  override lazy val additionalTypes: List[MaterializedType] = resolvers.flatMap {
    case AdditionalTypes(at) => at
    case _ => Nil
  }.toList

  def validateSchema(
      schema: ast.Document,
      validator: QueryValidator = ResolverBasedAstSchemaBuilder.validator,
      errorsLimit: Option[Int] = None): Vector[Violation] =
    allowKnownDynamicDirectives(
      validator.validateQuery(validationSchema, schema, Map.empty, errorsLimit))

  def validateSchemaWithException(
      schema: ast.Document,
      validator: QueryValidator = ResolverBasedAstSchemaBuilder.validator)
      : ResolverBasedAstSchemaBuilder[Ctx] = {
    val violations = validateSchema(schema, validator)

    if (violations.nonEmpty) throw MaterializedSchemaValidationError(violations)
    else this
  }

  protected def allowKnownDynamicDirectives(violations: Vector[Violation]) =
    violations.filterNot {
      case UnknownDirectiveViolation(name, _, _) if dynamicDirectiveNames.contains(name) => true
      case _ => false
    }

  protected def findResolver(
      directive: ast.Directive): Option[(ast.Directive, AstSchemaResolver[Ctx])] =
    resolvers.collectFirst {
      case r @ DirectiveResolver(d, _, _) if d.name == directive.name => directive -> r
      case r @ DynamicDirectiveResolver(directive.name, _, _) => directive -> r
    }

  protected def findComplexityResolver(
      directive: ast.Directive): Option[(ast.Directive, AstSchemaResolver[Ctx])] =
    resolvers.collectFirst {
      case r @ DirectiveResolver(d, _, _) if d.name == directive.name && r.complexity.isDefined =>
        directive -> r
      case r @ DynamicDirectiveResolver(directive.name, _, _) if r.complexity.isDefined =>
        directive -> r
    }

  protected def findResolver(
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
      definition: ast.FieldDefinition): Option[FieldResolver[Ctx]] = {
    val arg = typeDefinition -> definition

    resolvers.collectFirst {
      case r @ FieldResolver(fn, _) if fn.isDefinedAt(arg) => r
    }
  }

  protected def findComplexityResolver(
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
      definition: ast.FieldDefinition): Option[FieldResolver[Ctx]] = {
    val arg = typeDefinition -> definition

    resolvers.collectFirst {
      case r @ FieldResolver(fn, _) if fn.isDefinedAt(arg) && r.complexity.isDefinedAt(arg) => r
    }
  }

  protected def findExistingResolver(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _]],
      field: Field[Ctx, _]): Option[ExistingFieldResolver[Ctx]] = {
    val arg = (origin, typeDefinition, field)

    resolvers.collectFirst {
      case r @ ExistingFieldResolver(fn) if fn.isDefinedAt(arg) => r
    }
  }

  protected def findAnyResolver(origin: MatOrigin): Option[AnyFieldResolver[Ctx]] =
    resolvers.collectFirst {
      case r @ AnyFieldResolver(fn) if fn.isDefinedAt(origin) => r
    }

  override def buildSchema(
      definition: Option[ast.SchemaDefinition],
      extensions: List[ast.SchemaExtensionDefinition],
      queryType: ObjectType[Ctx, Any],
      mutationType: Option[ObjectType[Ctx, Any]],
      subscriptionType: Option[ObjectType[Ctx, Any]],
      additionalTypes: List[Type with Named],
      directives: List[Directive],
      mat: AstSchemaMaterializer[Ctx]) =
    Schema[Ctx, Any](
      query = queryType,
      mutation = mutationType,
      subscription = subscriptionType,
      additionalTypes = additionalTypes,
      description = definition.flatMap(_.description.map(_.value)),
      directives = directives,
      astDirectives =
        definition.fold(Vector.empty[ast.Directive])(_.directives) ++ extensions.flatMap(
          _.directives),
      astNodes = Vector(mat.document) ++ extensions ++ definition.toVector,
      validationRules = SchemaValidationRule.default :+ new ResolvedDirectiveValidationRule(
        this.directives.filterNot(_.repeatable).map(_.name).toSet)
    )

  override def resolveField(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      definition: ast.FieldDefinition,
      mat: AstSchemaMaterializer[Ctx]
  ): Context[Ctx, _] => Action[Ctx, _] = {
    val dResolvers = definition.directives.flatMap(findResolver(_))

    if (dResolvers.nonEmpty)
      c => {
        val resultAction =
          dResolvers.foldLeft(None: Option[Action[Ctx, Any]]) {
            case (acc, (d, DirectiveResolver(sd, fn, _))) =>
              Some(
                fn(
                  AstDirectiveContext[Ctx](
                    d,
                    typeDefinition,
                    definition,
                    extensions,
                    c,
                    acc,
                    Args(sd, d))))

            case (acc, (d, ddc @ DynamicDirectiveResolver(_, fn, _))) =>
              implicit val marshaller = ddc.marshaller

              Some(
                fn(
                  DynamicDirectiveContext(
                    d,
                    typeDefinition,
                    definition,
                    extensions,
                    c,
                    acc,
                    ResolverBasedAstSchemaBuilder.createDynamicArgs(d))))

            case (acc, _) =>
              acc
          }

        val typeDefinitionName = typeDefinition match {
          case Left(t) => t.name
          case Right(t) => t.name
        }

        resultAction.getOrElse(
          throw SchemaMaterializationException(
            s"Resolver for '$typeDefinitionName.${definition.name}' haven't returned any action!"))
      }
    else
      findResolver(typeDefinition, definition) match {
        case Some(fResolver) =>
          fResolver.resolve(typeDefinition -> definition)
        case None =>
          findAnyResolver(origin) match {
            case Some(fResolver) =>
              fResolver.resolve(origin)
            case None =>
              super.resolveField(origin, typeDefinition, extensions, definition, mat)
          }
      }
  }

  override def extendFieldResolver(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _]],
      existing: Field[Ctx, Any],
      fieldType: OutputType[_],
      mat: AstSchemaMaterializer[Ctx]) =
    findExistingResolver(origin, typeDefinition, existing) match {
      case Some(fResolver) =>
        fResolver.resolve((origin, typeDefinition, existing))
      case None =>
        findAnyResolver(origin) match {
          case Some(fResolver) =>
            fResolver.resolve(origin)
          case None =>
            super.extendFieldResolver(origin, typeDefinition, existing, fieldType, mat)
        }
    }

  override def buildFieldType(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      definition: ast.FieldDefinition,
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx]
  ) = {
    val ctx = AstOutputTypeContext(origin, typeDefinition, definition, extensions, mat)

    val tpe =
      resolvers.collectFirst {
        case DirectiveOutputTypeResolver(d, fn) if definition.directives.exists(_.name == d.name) =>
          val astDirective = definition.directives.find(_.name == d.name).get

          fn(
            AstDirectiveOutputTypeContext(
              origin,
              astDirective,
              typeDefinition,
              definition,
              extensions,
              mat,
              Args(d, astDirective)))

        case OutputTypeResolver(fn) if fn.isDefinedAt(ctx) => fn(ctx)
      }

    tpe.getOrElse(
      super.buildFieldType(origin, typeDefinition, extensions, definition, arguments, mat))
  }

  private def buildInputTypeResolver(
      origin: MatOrigin,
      schemaDefinition: Option[Type with Named],
      astDefinition: Option[ast.TypeSystemDefinition],
      astField: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition,
      mat: AstSchemaMaterializer[Ctx]) = {
    val ctx =
      AstInputTypeContext(origin, schemaDefinition, astDefinition, astField, definition, mat)

    resolvers.collectFirst {
      case DirectiveInputTypeResolver(d, fn) if definition.directives.exists(_.name == d.name) =>
        val astDirective = definition.directives.find(_.name == d.name).get

        fn(
          AstDirectiveInputTypeContext(
            origin,
            astDirective,
            schemaDefinition,
            astDefinition,
            astField,
            definition,
            mat,
            Args(d, astDirective)))

      case InputTypeResolver(fn) if fn.isDefinedAt(ctx) => fn(ctx)
    }
  }

  override def buildArgumentType(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeSystemDefinition, ObjectLikeType[Ctx, _]],
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition,
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx]) =
    buildInputTypeResolver(
      origin,
      typeDefinition.toOption,
      typeDefinition.left.toOption,
      fieldDefinition,
      definition,
      mat).getOrElse(
      super
        .buildArgumentType(origin, typeDefinition, fieldDefinition, definition, defaultValue, mat))

  override def buildInputFieldType(
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      typeDefinition: Either[ast.InputObjectTypeDefinition, InputObjectType[_]],
      definition: ast.InputValueDefinition,
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx]) =
    buildInputTypeResolver(
      origin,
      typeDefinition.toOption,
      typeDefinition.left.toOption,
      None,
      definition,
      mat).getOrElse(
      super.buildInputFieldType(origin, extensions, typeDefinition, definition, defaultValue, mat))

  override def buildScalarType(
      origin: MatOrigin,
      extensions: Vector[ast.ScalarTypeExtensionDefinition],
      definition: ast.ScalarTypeDefinition,
      mat: AstSchemaMaterializer[Ctx]
  ) = {
    val scalar =
      resolvers.collectFirst {
        case DirectiveScalarResolver(d, fn) if definition.directives.exists(_.name == d.name) =>
          val astDirective = definition.directives.find(_.name == d.name).get

          fn(AstDirectiveScalarContext(astDirective, definition, Args(d, astDirective)))

        case ScalarResolver(fn) if fn.isDefinedAt(definition) =>
          fn(definition)
      }

    scalar match {
      case Some(s) => Some(s.asInstanceOf[ScalarType[Any]])
      case _ => super.buildScalarType(origin, extensions, definition, mat)
    }
  }

  override def resolveNameConflict(fromOrigin: MatOrigin, types: Vector[MaterializedType]) =
    resolvers
      .collectFirst { case r: ConflictResolver[Ctx] => r.resolve(fromOrigin, types) }
      .getOrElse(super.resolveNameConflict(fromOrigin, types))

  override def fieldComplexity(
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
      definition: ast.FieldDefinition) = {
    val dResolvers = definition.directives.flatMap(findComplexityResolver(_))
    val fromDirectives =
      dResolvers.foldLeft(None: Option[(Ctx, Args, Double) => Double]) {
        case (None, (d, DirectiveResolver(sd, _, Some(complexity)))) =>
          Some(
            complexity(ComplexityDirectiveContext[Ctx](d, typeDefinition, definition, Args(sd, d))))

        case (None, (d, ddc @ DynamicDirectiveResolver(_, _, Some(complexity)))) =>
          implicit val marshaller = ddc.marshaller

          Some(
            complexity(
              ComplexityDynamicDirectiveContext(
                d,
                typeDefinition,
                definition,
                ResolverBasedAstSchemaBuilder.createDynamicArgs(d))))

        case (acc, _) =>
          acc
      }

    fromDirectives
      .orElse(
        findComplexityResolver(typeDefinition, definition).map(
          _.complexity(typeDefinition -> definition)))
      .orElse(super.fieldComplexity(typeDefinition, definition))
  }

  override def buildAdditionalFields(
      origin: MatOrigin,
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      typeDefinition: ast.TypeDefinition,
      mat: AstSchemaMaterializer[Ctx]
  ) = {
    val allAstDirectives = typeDefinition.directives ++ extensions.flatMap(_.directives)

    val materializedFields =
      allAstDirectives.flatMap { astDir =>
        resolvers.collect {
          case DirectiveFieldProvider(directive, resolve) if directive.name == astDir.name =>
            resolve(
              DirectiveFieldProviderContext[Ctx](
                origin,
                astDir,
                typeDefinition,
                extensions,
                mat,
                Args(directive, astDir)))

          case ddfp @ DynamicDirectiveFieldProvider(astDir.name, resolve) =>
            implicit val marshaller = ddfp.marshaller

            resolve(
              DynamicDirectiveFieldProviderContext(
                origin,
                astDir,
                typeDefinition,
                extensions,
                mat,
                ResolverBasedAstSchemaBuilder.createDynamicArgs(astDir)))
        }
      }

    materializedFields.flatten.toList.asInstanceOf[List[MaterializedField[Ctx, Any]]] ++
      super.buildAdditionalFields(origin, extensions, typeDefinition, mat)
  }

  override def transformScalarType[T](
      origin: MatOrigin,
      extensions: Vector[ast.ScalarTypeExtensionDefinition],
      existing: ScalarType[T],
      mat: AstSchemaMaterializer[Ctx]
  ) = {
    val ctx = ExistingScalarContext(origin, extensions, existing.asInstanceOf[ScalarType[Any]], mat)

    val resolved = resolvers.collectFirst {
      case ExistingScalarResolver(resolve) if resolve.isDefinedAt(ctx) =>
        resolve(ctx).asInstanceOf[ScalarType[T]]
    }

    resolved.getOrElse(super.transformScalarType(origin, extensions, existing, mat))
  }

  override def transformEnumType[T](
      origin: MatOrigin,
      extensions: Vector[ast.EnumTypeExtensionDefinition],
      existing: EnumType[T],
      mat: AstSchemaMaterializer[Ctx]
  ) = {
    val ctx =
      ExistingEnumContext[Ctx](origin, extensions, existing.asInstanceOf[EnumType[Any]], mat)

    val resolved = resolvers.collectFirst {
      case ExistingEnumResolver(resolve) if resolve.isDefinedAt(ctx) =>
        resolve(ctx).asInstanceOf[EnumType[T]]
    }

    resolved.getOrElse(super.transformEnumType(origin, extensions, existing, mat))
  }

  override def objectTypeInstanceCheck(
      origin: MatOrigin,
      definition: ast.ObjectTypeDefinition,
      extensions: List[ast.ObjectTypeExtensionDefinition]): Option[(Any, Class[_]) => Boolean] = {
    val ctx = InstanceCheckContext[Ctx](origin, definition, extensions)

    resolvers.collectFirst { case InstanceCheck(fn) =>
      fn(ctx)
    }
  }

  override def extendedObjectTypeInstanceCheck(
      origin: MatOrigin,
      tpe: ObjectType[Ctx, _],
      extensions: List[ast.ObjectTypeExtensionDefinition]): Option[(Any, Class[_]) => Boolean] = {
    val ctx = ExistingInstanceCheckContext[Ctx](origin, tpe, extensions)

    resolvers.collectFirst { case ExistingInstanceCheck(fn) =>
      fn(ctx)
    }
  }

  override def enumValue(
      typeDefinition: Either[ast.EnumTypeDefinition, EnumType[_]],
      definition: ast.EnumValueDefinition) = {
    val ctx = typeDefinition -> definition
    val resolved =
      resolvers.collectFirst {
        case SimpleEnumValueResolver(fn) if fn.isDefinedAt(ctx) => fn(ctx)
      }

    resolved.getOrElse(super.enumValue(typeDefinition, definition))
  }
}

object ResolverBasedAstSchemaBuilder {
  def apply[Ctx](resolvers: AstSchemaResolver[Ctx]*) =
    new ResolverBasedAstSchemaBuilder[Ctx](resolvers)

  val validator: QueryValidator = QueryValidator.ruleBased(
    QueryValidator.allRules.filterNot(_.isInstanceOf[ExecutableDefinitions]))

  private def invalidType[In](expected: String, got: In)(implicit iu: InputUnmarshaller[In]) =
    throw InputMaterializationException(s"Expected $expected value, but got: " + iu.render(got))

  private def safe[T, In](op: => T, expected: String, got: In)(implicit iu: InputUnmarshaller[In]) =
    try op
    catch {
      case NonFatal(_) => invalidType(expected, got)
    }

  private def extractScalar[In](t: ScalarType[_], value: In)(implicit iu: InputUnmarshaller[In]) = {
    val coerced = iu.getScalarValue(value)

    t match {
      case BooleanType =>
        coerced match {
          case v: Boolean => v
          case v: String => safe(v.toBoolean, "Boolean", value)
          case _ => invalidType("Boolean", value)
        }
      case StringType =>
        coerced.toString
      case IDType =>
        coerced match {
          case s: String => s
          case _ => invalidType("ID", value)
        }
      case IntType =>
        coerced match {
          case v: Int => v
          case i: Long if i.isValidInt => i.toInt
          case v: BigInt if v.isValidInt => v.intValue
          case d: Double if d.isValidInt => d.intValue
          case d: BigDecimal if d.isValidInt => d.intValue
          case v: String => safe(v.toInt, "Int", value)
          case _ => invalidType("Int", value)
        }
      case LongType =>
        coerced match {
          case i: Int => i: Long
          case i: Long => i
          case i: BigInt if !i.isValidLong => invalidType("Long", value)
          case i: BigInt => i.longValue
          case d: Double if d.isWhole => d.toLong
          case d: BigDecimal if d.isValidLong => d.longValue
          case v: String => safe(v.toLong, "Long", value)
          case _ => invalidType("Long", value)
        }
      case BigIntType =>
        coerced match {
          case i: Int => BigInt(i)
          case i: Long => BigInt(i)
          case i: BigInt => i
          case d: Double if d.isWhole => BigInt(d.toLong)
          case d: BigDecimal if d.isWhole => d.toBigInt
          case v: String => safe(BigInt(v), "BigInt", value)
          case _ => invalidType("BigInt", value)
        }
      case BigDecimalType =>
        coerced match {
          case i: Int => BigDecimal(i)
          case i: Long => BigDecimal(i)
          case i: BigInt => BigDecimal(i)
          case d: Double => BigDecimal(d)
          case d: BigDecimal => d
          case v: String => safe(BigDecimal(v), "BigDecimal", value)
          case _ => invalidType("BigDecimal", value)
        }
      case FloatType =>
        coerced match {
          case i: Int => i.toDouble
          case i: Long => i.toDouble
          case i: BigInt if !i.isValidDouble => invalidType("Float", value)
          case i: BigInt => i.doubleValue
          case d: Double => d
          case d: BigDecimal if !d.isDecimalDouble => invalidType("Float", value)
          case d: BigDecimal => d.doubleValue
          case v: String => safe(v.toDouble, "Float", value)
          case _ => invalidType("Float", value)
        }
      case _ => t.coerceUserInput(coerced).fold(e => invalidType(t.name, value), identity)
    }
  }

  private def extractEnum[In](t: EnumType[_], value: In)(implicit iu: InputUnmarshaller[In]) =
    iu.getScalarValue(value).toString

  def extractValue[In](tpe: OutputType[_], value: Option[In])(implicit
      iu: InputUnmarshaller[In]): Any = tpe match {
    case OptionType(ofType) => Option(extractValue(ofType, value))
    case _ if value.isEmpty || !iu.isDefined(value.get) => null
    case ListType(ofType) => iu.getListValue(value.get).map(v => extractValue(ofType, Some(v)))
    case t: ScalarAlias[_, _] => extractValue(t.aliasFor, value)
    case t: ScalarType[_] => extractScalar(t, value.get)
    case t: EnumType[_] => extractEnum(t, value.get)
    case _: CompositeType[_] =>
      val objValue = value.get

      if (iu.isMapNode(objValue)) objValue
      else invalidType("Object", objValue)
  }

  def extractFieldValue[Ctx, In](context: Context[Ctx, _])(implicit
      iu: InputUnmarshaller[In]): Any =
    extractFieldValue[In](context.parentType, context.field, context.value.asInstanceOf[In])

  def extractFieldValue[In](parentType: CompositeType[_], field: Field[_, _], value: In)(implicit
      iu: InputUnmarshaller[In]): Any =
    try
      if (!iu.isMapNode(value))
        throw SchemaMaterializationException(
          s"Can't extract value for a field '${parentType.name}.${field.name}': not a map-like value.")
      else
        extractValue(field.fieldType, iu.getMapValue(value, field.name))
    catch {
      case e: SchemaMaterializationException =>
        throw e

      case NonFatal(e) =>
        throw SchemaMaterializationException(
          s"Can't extract value for a field '${parentType.name}.${field.name}'.",
          e)
    }

  def defaultInputResolver[Ctx, In: InputUnmarshaller] =
    FieldResolver[Ctx](
      { case (_, _) =>
        extractFieldValue[Ctx, In]
      },
      PartialFunction.empty)

  def defaultExistingInputResolver[Ctx, In: InputUnmarshaller] =
    ExistingFieldResolver[Ctx] { case (_, _, _) =>
      extractFieldValue[Ctx, In]
    }

  def defaultAnyInputResolver[Ctx, In: InputUnmarshaller] =
    AnyFieldResolver[Ctx] {
      case origin if !origin.isInstanceOf[ExistingSchemaOrigin[_, _]] => extractFieldValue[Ctx, In]
    }

  def resolveDirectives[T](
      schema: ast.Document,
      resolvers: AstSchemaGenericResolver[T]*): Vector[T] = {
    val result = new VectorBuilder[T]
    val resolversByName = resolvers.groupBy(_.directiveName)
    val stack = ValidatorStack.empty[ast.AstNode]

    ast.AstVisitor.visit(
      schema,
      ast.AstVisitor(
        onEnter = {
          case node: ast.WithDirectives =>
            stack.push(node)

            result ++=
              node.directives.flatMap { astDir =>
                findByLocation(stack, node, resolversByName.getOrElse(astDir.name, Nil))
                  .flatMap {
                    case GenericDirectiveResolver(directive, _, resolve) =>
                      resolve(GenericDirectiveContext(astDir, node, Args(directive, astDir)))
                    case gd @ GenericDynamicDirectiveResolver(_, _, resolve) =>
                      implicit val marshaller = gd.marshaller

                      resolve(
                        GenericDynamicDirectiveContext(
                          astDir,
                          node,
                          createDynamicArgs(astDir)(marshaller)))
                  }
              }

            VisitorCommand.Continue

          case node =>
            stack.push(node)
            VisitorCommand.Continue
        },
        onLeave = { case _ =>
          stack.pop()
          VisitorCommand.Continue
        }
      )
    )

    result.result()
  }

  def createDynamicArgs[T: ResultMarshallerForType](astDirective: ast.Directive): T = {
    import sangria.marshalling.queryAst._
    import sangria.marshalling.MarshallingUtil._

    val value: ast.Value =
      ast.ObjectValue(astDirective.arguments.map(arg => ast.ObjectField(arg.name, arg.value)))

    value.convertMarshaled[T]
  }

  private def findByLocation[T](
      visitorStack: ValidatorStack[ast.AstNode],
      node: ast.AstNode,
      directives: Seq[AstSchemaGenericResolver[T]]) =
    directives.filter(d =>
      d.locations.isEmpty || KnownDirectives
        .getLocation(node, visitorStack.head(1))
        .fold(false)(l => d.locations contains l._1))
}
