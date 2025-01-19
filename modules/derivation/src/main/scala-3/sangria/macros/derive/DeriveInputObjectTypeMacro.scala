package sangria.macros.derive

import scala.quoted._

import sangria.schema.{Field, InputField}
import sangria.marshalling.ToInput
import sangria.schema.WithoutInputTypeTags
import sangria.schema.InputType
import scala.annotation.tailrec

object DeriveInputObjectTypeMacro {
  def deriveInputObjectType[T](using
      Quotes,
      Type[T])(config: Expr[Seq[DeriveInputObjectSetting]]) = {
    val Varargs(configSeq) = config
    new DeriveInputObjectTypeMacro().deriveInputObjectType(configSeq)
  }
}

// For convenience in handling data structures we include a top level
// Quotes instance in the class directly
private class DeriveInputObjectTypeMacro(using globalQuotes: Quotes) extends DeriveMacroSupport {

  def deriveInputObjectType[T](using targetType: Type[T])(
      config: Seq[Expr[DeriveInputObjectSetting]]) = {
    import globalQuotes.reflect._
    val validatedConfig = validateObjectConfig(config)

    val errors = validatedConfig.collect { case Left(error) => error }

    if (errors.nonEmpty) reportErrors(errors)
    else {
      val validConfig = validatedConfig.collect { case Right(cfg) => cfg }

      collectFields(validConfig) match {
        case Left(errors) => reportErrors(errors)
        case Right(fields) =>
          val tpeName = Expr(TypeRepr.of[T].typeSymbol.name)

          val annotationName = symbolName(TypeRepr.of[T].typeSymbol.annotations)
          val configName = validConfig.collect { case MacroName(name) => name }.lastOption

          val annotationDesc = symbolDescription(TypeRepr.of[T].typeSymbol.annotations)
          val configDesc = validConfig.collect { case MacroDescription(name) => name }.lastOption

          '{
            sangria.schema.InputObjectType.createFromMacro[targetType.Underlying](
              ${ configName.orElse(annotationName).getOrElse(tpeName) },
              ${ flattenOptionExpr[String](configDesc.orElse(annotationDesc)) },
              () => ${ Expr.ofList(fields) })
          }
      }
    }
  }

  private def findApplyMethod[T](using Type[T]): Either[
    (PositionPointer, String),
    Option[(globalQuotes.reflect.Symbol, globalQuotes.reflect.Symbol)]] = {
    import globalQuotes.reflect._
    val typeShow = TypeRepr.of[T].show
    val companion = TypeRepr.of[T].typeSymbol.companionModule
    if (companion == Symbol.noSymbol) {
      Left(PositionByQuotes(
        globalQuotes) -> s"Can't find companion object for '${typeShow}'. This can happen when it's nested too deeply. Please consider defining it as a top-level object or directly inside of another class or object.")
    } else {
      val applyMethods = companion.methodMembers.collect {
        case m: Symbol if m.name == "apply" => m
      }

      if (applyMethods.size > 1)
        Left(
          PositionByQuotes(
            globalQuotes) -> "Companion object has more than one `apply` method, which is not supported.")
      else
        Right(Some(companion -> applyMethods.head))
    }
  }

  private def collectFields[T](using targetType: Type[T])(config: Seq[MacroSetting])
      : Either[List[(PositionPointer, String)], List[Expr[InputField[_]]]] =
    import globalQuotes.reflect._
    findApplyMethod[T] match {
      case Right(apply) =>
        val knownMembers = findKnownMembers[T](apply)

        validateFieldConfig(knownMembers, config) match {
          case Nil =>
            val fields = extractFields(knownMembers, config)

            val classFields = fields.map { field =>

              @tailrec
              def methodResultType(typeRepr: TypeRepr): TypeRepr = typeRepr match
                case MethodType(_, _, retType) => methodResultType(retType)
                case retType => retType

              // Contextualize the method type with respect to the enclosing type
              val fieldType = methodResultType(field.onType.memberType(field.method).widen)

              val annotationType = symbolInputType(field.annotations)

              val name = field.name
              val annotationName = symbolName(field.annotations)
              val configName = config.collect { case MacroRenameField(`name`, expr, _) =>
                expr
              }.lastOption

              val annotationDescr = symbolDescription(field.annotations)
              val configDescr = config.collect { case MacroDocumentField(`name`, expr, _) =>
                expr
              }.lastOption

              val defaultAnnotation = symbolDefault(field.annotations)
              val defaultSig = field.defaultValue.map { case (comp, default) =>
                (Ref(default).tpe.widen, Ref(comp).select(default).asExpr)
              }
              val default = defaultAnnotation.orElse(defaultSig)

              val fieldName: Expr[String] = {
                val nonTransformedName = configName.orElse(annotationName).getOrElse(Expr(name))

                config.collect { case MacroTransformFieldNames(fnt) => fnt }.lastOption match {
                  case Some(fnt) => '{ $fnt($nonTransformedName) }
                  case None => nonTransformedName
                }
              }
              fieldType.asType match
                case '[f] =>
                  default match {
                    case Some((defaultType, defaultValue)) =>
                      val ft = annotationType.getOrElse {
                        if (fieldType <:< TypeRepr.of[Option[_]])
                          Expr.summon[sangria.macros.derive.GraphQLInputTypeLookup[f, _]] match
                            case Some(lookup) => '{ $lookup.graphqlType }
                            case None =>
                              reportSummoningErrors(
                                Seq(s"GraphQlInputType not found: ${TypeRepr.of[f].show}"),
                                Seq(None)
                              )
                        else
                          Expr.summon[
                            sangria.macros.derive.GraphQLInputTypeLookup[Option[f], _]] match
                            case Some(lookup) => '{ $lookup.graphqlType }
                            case None =>
                              reportSummoningErrors(
                                Seq(s"GraphQlInputType not found: ${TypeRepr.of[Option[f]].show}"),
                                Seq(None)
                              )
                      }

                      defaultType.asType match
                        case '[d] =>
                          ft.asTerm.tpe.widen.typeArgs.head.asType match
                            case '[t] =>
                              (
                                Expr.summon[ToInput[d, _]],
                                Expr.summon[WithoutInputTypeTags[t]]) match
                                case (Some(toInput), Some(weakInputTypeTags)) =>
                                  '{
                                    sangria.schema.InputField.createFromMacroWithDefault[t, d](
                                      $fieldName,
                                      ${ ft.asExprOf[InputType[t]] },
                                      ${
                                        flattenOptionExpr[String](
                                          configDescr.orElse(annotationDescr))
                                      },
                                      ${ defaultValue.asExprOf[d] })($toInput, $weakInputTypeTags)
                                  }
                                case optionTuple =>
                                  reportSummoningErrors(
                                    Seq(
                                      s"Implicit not found: ${TypeRepr.of[ToInput[d, _]].show}",
                                      s"Implicit not found: ${TypeRepr.of[WithoutInputTypeTags[t]].show}"
                                    ),
                                    optionTuple.toList
                                  )
                    case None =>
                      val graphQlType = annotationType.getOrElse {
                        Expr.summon[sangria.macros.derive.GraphQLInputTypeLookup[f, _]] match
                          case Some(lookup) => '{ $lookup.graphqlType }
                          case None =>
                            reportSummoningErrors(
                              Seq(s"GraphQlInputType not found: ${TypeRepr.of[f].show}"),
                              Seq(None)
                            )
                      }
                      '{
                        sangria.schema.InputField.createFromMacroWithoutDefault(
                          $fieldName,
                          ${ graphQlType },
                          ${ flattenOptionExpr[String](configDescr.orElse(annotationDescr)) })
                      }
                  }
            }

            val allFields = classFields ++ additionalFields(config)

            if (allFields.nonEmpty) Right(allFields)
            else Left(List(PositionByQuotes(globalQuotes) -> "Input field list is empty"))
          case errors => Left(errors)
        }
      case Left(error) => reportErrors(error :: Nil)
    }

  private def findKnownMembers[T](using Type[T])(
      apply: Option[(globalQuotes.reflect.Symbol, globalQuotes.reflect.Symbol)])
      : List[KnownMember] = {
    import globalQuotes.reflect._
    TypeRepr
      .of[T]
      .typeSymbol
      .caseFields
      .map { m =>
        val (annotations, default) = findCaseClassAccessorAnnotations[T](m, apply)
        KnownMember(TypeRepr.of[T], m, annotations, default)
      }
      .toList
  }

  private def findCaseClassAccessorAnnotations[T](using Type[T])(
      member: globalQuotes.reflect.Symbol,
      applyInfo: Option[(globalQuotes.reflect.Symbol, globalQuotes.reflect.Symbol)]
  ): (
      List[globalQuotes.reflect.Term],
      Option[(globalQuotes.reflect.Symbol, globalQuotes.reflect.Symbol)]) = {
    import globalQuotes.reflect._
    applyInfo match {
      case Some((companion, apply)) =>
        val annotationsConstructors =
          for {
            c <- TypeRepr.of[T].widen.typeSymbol.primaryConstructor.paramSymss
            p <- c
            if p.name == member.name
          } yield p.annotations

        val defaults =
          companion.companionClass.caseFields.zipWithIndex
            .find(_._1.name == member.name) match {
            case Some((param: Symbol, idx)) if param.flags.is(Flags.HasDefault) =>
              Some(
                companion -> companion
                  .methodMember(s"$$lessinit$$greater$$default$$${idx + 1}")
                  .head
              )
            case _ => None
          }

        annotationsConstructors.toList.flatten -> defaults

      case None =>
        Nil -> None
    }
  }

  private def extractFields(knownMembers: List[KnownMember], config: Seq[MacroSetting]) = {
    val included = config.foldLeft(Set.empty[String]) {
      case (acc, MacroIncludeFields(fields, _)) => acc ++ fields
      case (acc, _) => acc
    }

    val excluded = config.foldLeft(Set.empty[String]) {
      case (acc, MacroExcludeFields(fields, _)) => acc ++ fields
      case (acc, MacroReplaceField(fieldName, _, _)) => acc + fieldName
      case (acc, _) => acc
    }

    val actualIncluded =
      if (included.nonEmpty) included
      else knownMembers.map(m => m.name).toSet

    val actualFields = actualIncluded -- excluded

    knownMembers.filter(m => actualFields.contains(m.name) && !memberExcluded(m.annotations))
  }

  private def additionalFields(config: Seq[MacroSetting]) =
    config.foldLeft(List[Expr[InputField[_]]]()) {
      case (acc, MacroReplaceField(_, field, _)) => acc :+ field
      case (acc, _) => acc
    }

  private def validateFieldConfig(knownMembers: List[KnownMember], config: Seq[MacroSetting]) = {
    val knownMembersSet = knownMembers.map(_.name).toSet

    def unknownMember(pos: PositionPointer, name: String) =
      pos -> s"Unknown member '$name'. Known members are: ${knownMembers.map(_.name).mkString(", ")}"

    config.toList.flatMap {
      case MacroIncludeFields(fields, pos) if !fields.forall(knownMembersSet.contains) =>
        val unknown = fields.diff(knownMembersSet)

        unknown.toList.map(unknownMember(pos, _))

      case MacroExcludeFields(fields, pos) if !fields.forall(knownMembersSet.contains) =>
        val unknown = fields.diff(knownMembersSet)

        unknown.toList.map(unknownMember(pos, _))

      case MacroDocumentField(fieldName, _, pos) if !knownMembersSet.contains(fieldName) =>
        unknownMember(pos, fieldName) :: Nil

      case MacroRenameField(fieldName, _, pos) if !knownMembersSet.contains(fieldName) =>
        unknownMember(pos, fieldName) :: Nil

      case MacroReplaceField(fieldName, _, pos) if !knownMembersSet.contains(fieldName) =>
        unknownMember(pos, fieldName) :: Nil

      case _ => Nil
    }
  }

  private def validateObjectConfig[T](using Type[T])(config: Seq[Expr[DeriveInputObjectSetting]]) =
    config.map {
      case '{ InputObjectTypeName.apply($name) } =>
        Right(MacroName(name))

      case '{ InputObjectTypeDescription.apply($description) } =>
        Right(MacroDescription(description))

      case expr @ '{ DocumentInputField.apply($fieldName, $description) } =>
        Right(MacroDocumentField(fieldName.valueOrAbort, description, PositionByExpr(expr)))

      case expr @ '{ RenameInputField.apply($fieldName, $graphqlName) } =>
        Right(MacroRenameField(fieldName.valueOrAbort, graphqlName, PositionByExpr(expr)))

      case expr @ '{ ReplaceInputField.apply($fieldName, $field) } =>
        Right(MacroReplaceField(fieldName.valueOrAbort, field, PositionByExpr(expr)))

      case expr @ '{ IncludeInputFields.apply(${ Varargs(fields) }: _*) } =>
        Right(MacroIncludeFields(fields.map(_.valueOrAbort).toSet, PositionByExpr(expr)))

      case expr @ '{ ExcludeInputFields.apply(${ Varargs(fields) }: _*) } =>
        Right(MacroExcludeFields(fields.map(_.valueOrAbort).toSet, PositionByExpr(expr)))

      case '{ TransformInputFieldNames.apply($fn) } =>
        Right(MacroTransformFieldNames(fn))

      case expr =>
        Left(
          PositionByExpr(expr) ->
            "Unsupported shape of derivation config. Please define subclasses of `DeriveInputObjectTypeSetting` directly in the argument list of the macro.")
    }

  private case class KnownMember(
      onType: globalQuotes.reflect.TypeRepr,
      method: globalQuotes.reflect.Symbol,
      annotations: List[globalQuotes.reflect.Term],
      defaultValue: Option[(globalQuotes.reflect.Symbol, globalQuotes.reflect.Symbol)]) {
    lazy val name = method.name
  }

  private sealed trait Arg

  private case object ContextArg extends Arg
  private case class NormalArg(name: String, tpe: globalQuotes.reflect.TypeRepr, tree: Expr[Any])
      extends Arg

  private sealed trait MacroSetting

  private case class MacroName(name: Expr[String]) extends MacroSetting
  private case class MacroDescription(description: Expr[String]) extends MacroSetting

  private case class MacroDocumentField(
      fieldName: String,
      description: Expr[String],
      pos: PositionPointer)
      extends MacroSetting
  private case class MacroRenameField(
      fieldName: String,
      graphqlName: Expr[String],
      pos: PositionPointer)
      extends MacroSetting
  private case class MacroReplaceField(
      fieldName: String,
      field: Expr[InputField[_]],
      pos: PositionPointer)
      extends MacroSetting

  private case class MacroIncludeFields(fieldNames: Set[String], pos: PositionPointer)
      extends MacroSetting
  private case class MacroExcludeFields(fieldNames: Set[String], pos: PositionPointer)
      extends MacroSetting
  private case class MacroTransformFieldNames(transformer: Expr[String => String])
      extends MacroSetting
}
