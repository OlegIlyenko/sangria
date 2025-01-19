package sangria.macros.derive

import scala.quoted._
import sangria.schema.EnumType

object DeriveEnumTypeMacro extends DeriveMacroSupport {

  // Supports Scala 2 style enumeration only
  def deriveEnumType[T: Type](configSeq: Expr[Seq[DeriveEnumSetting]])(using
      Quotes): Expr[EnumType[T]] = {
    import quotes.reflect._
    val Varargs(config) = configSeq

    def collectEnumerationValues[T](using Quotes, Type[T]): List[Symbol] =
      val enumerationPath = TypeRepr.of[T].show.split("\\.").dropRight(1).mkString(".")
      Symbol
        .requiredModule(enumerationPath)
        .declaredFields
        .filter(s =>
          val flags = s.flags
          s.isTerm && !(flags.is(Flags.Method) || flags.is(Flags.Module)) && (s.typeRef <:< TypeRepr
            .of[Enumeration#Value]))
        .toList
        .reverse

    def collectKnownEnumSubtypes(s: Symbol): Either[(PositionPointer, String), List[Symbol]] =
      val flags = s.flags
      if (flags.is(Flags.Module)) Right(List(s))
      else {
        if ((flags.is(Flags.Trait) || flags.is(Flags.Abstract)) && flags.is(Flags.Sealed))
          s.children.foldLeft(Right(Nil): Either[(PositionPointer, String), List[Symbol]]) {
            case (Left(error), _) => Left(error)
            case (Right(set), knownSubclass) =>
              collectKnownEnumSubtypes(knownSubclass) match {
                case Left(error) => Left(error)
                case Right(subset) => Right(set ++ subset)
              }
          }
        else
          Left(
            PositionByQuotes(
              quotes) -> "Only `Enumeration` and sealed hierarchies of case objects are supported for GraphQL EnumType derivation.")
      }

    val t = summon[Type[T]]
    val validatedConfig = validateEnumConfig(config: _*)
    val errors = validatedConfig.collect { case Left(error) => error }

    if (errors.nonEmpty) reportErrors(errors)
    else {
      val validConfig = validatedConfig.collect { case Right(cfg) => cfg }
      val tRep = TypeRepr.of[T]

      val (tpe, validatedValues) =
        if (tRep <:< TypeRepr.of[Enumeration#Value]) {
          val enumerationPath = TypeRepr.of[T].show.split("\\.").dropRight(1).mkString(".")
          Symbol.requiredModule(enumerationPath).typeRef -> Right(collectEnumerationValues[T])
        } else {
          tRep -> collectKnownEnumSubtypes(tRep.typeSymbol)
        }

      validatedValues match {
        case Left(error) => reportErrors(error :: Nil)
        case Right(values) =>
          validateEnumValueConfig(values, validConfig) match {
            case Nil =>
              val tpeName = Expr(tpe.typeSymbol.name.toString)

              val annotationName = symbolName(tpe.typeSymbol.annotations)
              val configName = validConfig.collect { case MacroEnumTypeName(name) =>
                name
              }.lastOption

              val annotationDesc = symbolDescription(tpe.typeSymbol.annotations)
              val configDesc = validConfig.collect { case MacroEnumTypeDescription(name) =>
                name
              }.lastOption

              val enumValues = collectEnumValues[T](values, validConfig)

              '{
                sangria.schema.EnumType(
                  ${ configName.orElse(annotationName).getOrElse(tpeName) },
                  ${ flattenOptionExpr[String](configDesc.orElse(annotationDesc)) },
                  ${ Expr.ofList(enumValues) })
              }
            case configErrors => reportErrors(configErrors)
          }
      }
    }
  }

  private def collectEnumValues[T](using quotes: Quotes)(using Type[T])(
      values: List[quotes.reflect.Symbol],
      config: Seq[MacroDeriveEnumSetting]): List[Expr[sangria.schema.EnumValue[T]]] = {
    import quotes.reflect._
    val extractedValues = extractEnumValues(values, config)

    if (extractedValues.isEmpty)
      reportErrors(
        List(PositionByQuotes(quotes) -> "Enum value list is empty")
      )
    else
      extractedValues.sortBy(_.name).map { value =>
        val name = value.name
        val annotationName = symbolName(value.annotations)
        val configName = config.collect { case MacroRenameValue(`name`, expr, _) =>
          expr
        }.lastOption
        val actualName = {
          val nonTransformedName = configName.orElse(annotationName).getOrElse(Expr(name))
          val transformFnOpt = config.collect { case MacroTransformValueNames(fn) => fn }.lastOption
          val upperCase = config.exists(_.isInstanceOf[MacroUppercaseValues])

          val transformed =
            transformFnOpt
              .map(fn => '{ $fn($nonTransformedName) })
              .getOrElse(nonTransformedName)

          if (upperCase)
            '{ sangria.util.StringUtil.camelCaseToUnderscore($transformed).toUpperCase }
          else
            transformed
        }

        val annotationDescr = symbolDescription(value.annotations)
        val configDescr = config.collect { case MacroDocumentValue(`name`, expr, _, _) =>
          expr
        }.lastOption

        val annotationDepr = flattenOptionExpr[String](symbolDeprecation(value.annotations))
        val configDocDepr = config
          .collect { case MacroDocumentValue(`name`, _, reason, _) => reason }
          .lastOption
          .getOrElse(Expr(None))
        val configDepr = config
          .collect { case MacroDeprecateValue(`name`, reason, _) => reason }
          .lastOption
          .getOrElse(Expr(None))

        val valueFlags = value.flags

        val actualValue =
          if (valueFlags.is(Flags.Module) && !valueFlags.is(Flags.Package)) {
            val ownerFlags = value.owner.flags
            if (ownerFlags.is(Flags.Module) && !ownerFlags.is(Flags.Package)) {
              val typeSymbol = TypeRepr.of[T].typeSymbol.companionModule
              Select.unique(Ident(typeSymbol.termRef), value.name).asExprOf[T]
            } else {
              Ref(value).asExprOf[T]
            }
          } else {
            Ref(value).asExprOf[T]
          }

        '{
          sangria.schema.EnumValue[T](
            $actualName,
            ${ flattenOptionExpr[String](configDescr.orElse(annotationDescr)) },
            $actualValue,
            $configDocDepr.orElse($configDepr).orElse($annotationDepr)
          )
        }
      }
  }

  private def extractEnumValues(using
      quotes: Quotes)(values: List[quotes.reflect.Symbol], config: Seq[MacroDeriveEnumSetting]) = {
    val included = config.foldLeft(Set.empty[String]) {
      case (acc, MacroIncludeValues(vals, _)) => acc ++ vals
      case (acc, _) => acc
    }

    val excluded = config.foldLeft(Set.empty[String]) {
      case (acc, MacroExcludeValues(vals, _)) => acc ++ vals
      case (acc, _) => acc
    }

    val actualIncluded =
      if (included.nonEmpty) included
      else values.map(m => m.name.toString.trim).toSet

    val actualFields = actualIncluded -- excluded

    values.filter(m =>
      actualFields.contains(m.name.toString.trim) && !memberExcluded(m.annotations))
  }

  private def validateEnumValueConfig(using quotes: Quotes)(
      knownMembers: List[quotes.reflect.Symbol],
      config: Seq[MacroDeriveEnumSetting]) = {
    import quotes.reflect._
    val knownMembersSet = knownMembers.map(_.name.toString.trim).toSet

    def unknownMember(pos: PositionPointer, name: String) =
      pos -> s"Unknown enum value '$name'. Known members are: ${knownMembers.map(_.name.toString).mkString(", ")}"

    val valueValidations = config.toList.flatMap {
      case MacroIncludeValues(values, pos) if !values.forall(knownMembersSet.contains) =>
        val unknown = values.diff(knownMembersSet)

        unknown.toList.map(unknownMember(pos, _))

      case MacroExcludeValues(values, pos) if !values.forall(knownMembersSet.contains) =>
        val unknown = values.diff(knownMembersSet)

        unknown.toList.map(unknownMember(pos, _))

      case MacroDocumentValue(value, _, _, pos) if !knownMembersSet.contains(value) =>
        unknownMember(pos, value) :: Nil

      case MacroRenameValue(value, _, pos) if !knownMembersSet.contains(value) =>
        unknownMember(pos, value) :: Nil

      case MacroDeprecateValue(value, _, pos) if !knownMembersSet.contains(value) =>
        unknownMember(pos, value) :: Nil

      case _ => Nil
    }

    config.collect { case MacroUppercaseValues(pos) => pos }.lastOption match {
      case Some(pos) if config.exists(_.isInstanceOf[MacroRenameValue]) =>
        valueValidations :+ (pos -> "`UppercaseValues` is used together with `RenameValue` which is not allowed.")
      case _ =>
        valueValidations
    }
  }

  private def validateEnumConfig(config: Expr[DeriveEnumSetting]*)(using Quotes) = config.map {
    case '{ EnumTypeName($name) } =>
      Right(MacroEnumTypeName(name))

    case '{ EnumTypeDescription($description) } =>
      Right(MacroEnumTypeDescription(description))

    case expr @ '{ UppercaseValues } =>
      import quotes.reflect._
      Right(MacroUppercaseValues(PositionByExpr(expr)))

    case expr @ '{ DocumentValue($value, $description, $deprecationReason) } =>
      import quotes.reflect._
      Right(
        MacroDocumentValue(value.valueOrAbort, description, deprecationReason, PositionByExpr(expr))
      )

    case expr @ '{ RenameValue($value, $graphqlName) } =>
      import quotes.reflect._
      Right(MacroRenameValue(value.valueOrAbort, graphqlName, PositionByExpr(expr)))

    case expr @ '{ DeprecateValue($value, $deprecationReason) } =>
      import quotes.reflect._
      Right(
        MacroDeprecateValue(value.valueOrAbort, '{ Some($deprecationReason) }, PositionByExpr(expr))
      )

    case expr @ '{ IncludeValues(${ values }: _*) } =>
      import quotes.reflect._
      Right(MacroIncludeValues(values.valueOrAbort.toSet, PositionByExpr(expr)))

    case expr @ '{ ExcludeValues(${ values }: _*) } =>
      import quotes.reflect._
      Right(MacroExcludeValues(values.valueOrAbort.toSet, PositionByExpr(expr)))

    case expr @ '{ TransformValueNames($fn) } =>
      Right(MacroTransformValueNames(fn))

    case expr =>
      import quotes.reflect._
      Left(
        PositionByExpr(expr) ->
          "Unsupported shape of derivation config. Please define subclasses of `DeriveEnumTypeConfig` directly in the argument list of the macro.")
  }

  sealed trait MacroDeriveEnumSetting

  case class MacroEnumTypeName(name: Expr[String]) extends MacroDeriveEnumSetting
  case class MacroEnumTypeDescription(description: Expr[String]) extends MacroDeriveEnumSetting

  case class MacroUppercaseValues(pos: PositionPointer) extends MacroDeriveEnumSetting

  case class MacroDocumentValue(
      value: String,
      description: Expr[String],
      deprecationReason: Expr[Option[String]],
      pos: PositionPointer)
      extends MacroDeriveEnumSetting
  case class MacroDeprecateValue(
      value: String,
      deprecationReason: Expr[Option[String]],
      pos: PositionPointer)
      extends MacroDeriveEnumSetting
  case class MacroRenameValue(value: String, graphqlName: Expr[String], pos: PositionPointer)
      extends MacroDeriveEnumSetting

  case class MacroIncludeValues(values: Set[String], pos: PositionPointer)
      extends MacroDeriveEnumSetting
  case class MacroExcludeValues(fieldNames: Set[String], pos: PositionPointer)
      extends MacroDeriveEnumSetting

  case class MacroTransformValueNames(transformer: Expr[String => String])
      extends MacroDeriveEnumSetting
}
