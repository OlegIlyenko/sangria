package sangria.macros.derive

import sangria.execution.FieldTag
import sangria.execution.deferred.Deferred
import sangria.schema.{
  Action,
  ArgumentType,
  Context,
  Field,
  InputType,
  OutputType,
  PossibleInterface
}

import scala.concurrent.Future

import scala.quoted._
import sangria.schema.InterfaceType
import scala.reflect.ClassTag
import sangria.marshalling.{FromInput, ToInput}

import scala.language.implicitConversions
import scala.annotation.tailrec

object DeriveObjectTypeMacro {

  def deriveContextObjectType[Ctx, CtxVal, Val](using Quotes)(using
      ctx: Type[Ctx],
      ctxVal: Type[CtxVal],
      v: Type[Val]
  )(fn: Expr[Ctx => CtxVal], config: Expr[Seq[DeriveObjectSetting[Ctx, Val]]]) = {
    val Varargs(configSeq) = config
    new DeriveObjectTypeMacro().deriveObjectType(ctx, Some(ctxVal -> fn), v, configSeq)
  }

  def deriveNormalObjectType[Ctx, Val](using Quotes)(using
      ctx: Type[Ctx],
      v: Type[Val]
  )(config: Expr[Seq[DeriveObjectSetting[Ctx, Val]]]): Expr[sangria.schema.ObjectType[Ctx, Val]] = {
    val Varargs(configSeq) = config
    new DeriveObjectTypeMacro().deriveObjectType[Ctx, Any, Val](ctx, None, v, configSeq)
  }
}

// For convenience in handling data structures we include a top level
// Quotes instance in the class directly
class DeriveObjectTypeMacro(using globalQuotes: Quotes) extends DeriveMacroSupport {

  def deriveObjectType[Ctx, CtxVal, Val](
      ctxType: Type[Ctx],
      ctxValType: Option[(Type[CtxVal], Expr[Ctx => CtxVal])],
      valType: Type[Val],
      config: Seq[Expr[DeriveObjectSetting[Ctx, Val]]])(using
      Type[Ctx],
      Type[CtxVal],
      Type[Val]): Expr[sangria.schema.ObjectType[Ctx, Val]] = {
    import globalQuotes.reflect._
    val targetType = ctxValType.fold(valType)(_._1)
    val validatedConfig = validateObjectConfig(config, targetType)

    val errors = validatedConfig.collect { case Left(error) => error }

    if (errors.nonEmpty) reportErrors(errors)
    else {
      val validConfig = validatedConfig.collect { case Right(cfg) => cfg }

      collectFields(validConfig, ctxType, targetType, valType, ctxValType) match {
        case Left(errors) => reportErrors(errors)
        case Right(fields) =>
          val typeSymbol = targetType match
            case '[t] => TypeRepr.of[t].typeSymbol

          val tpeName = Expr(typeSymbol.name)

          val annotationName = symbolName(typeSymbol.annotations)
          val configName = validConfig.collect { case MacroName(name) => name }.lastOption

          val annotationDesc = symbolDescription(typeSymbol.annotations)
          val configDesc = validConfig.collect { case MacroDescription(name) => name }.lastOption

          val interfaces = validConfig.foldLeft(List[Expr[InterfaceType[Ctx, _]]]()) {
            case (acc, MacroInterfaces(exprs)) =>
              acc ++ exprs.map(i => unsafeSelectByName[InterfaceType[Ctx, _]](i, "interfaceType"))
            case (acc, _) => acc
          }

          '{
            sangria.schema.ObjectType.createFromMacro(
              ${ configName.orElse(annotationName).getOrElse(tpeName) },
              ${ flattenOptionExpr[String](configDesc.orElse(annotationDesc)) },
              ${ Expr.ofList(interfaces) },
              () => ${ Expr.ofList(fields) }
            )(${ getClassTag[Val] })
          }
      }
    }
  }

  private def collectFields[Ctx, T, Val, CtxVal](
      config: Seq[MacroDeriveObjectSetting],
      ctxType: Type[Ctx],
      targetType: Type[T],
      valType: Type[Val],
      ctxValType: Option[(Type[CtxVal], Expr[Ctx => CtxVal])])
      : Either[List[(PositionPointer, String)], List[Expr[sangria.schema.Field[Ctx, Val]]]] = {
    val knownMembers = findKnownMembers(
      targetType,
      config.foldLeft(Set.empty[String]) {
        case (acc, MacroIncludeMethods(methods)) => acc ++ methods
        case (acc, _) => acc
      })

    given Type[Ctx] = ctxType
    import globalQuotes.reflect._
    given Type[Val] = valType

    validateFieldConfig(knownMembers, config) match {
      case Nil =>
        val fields = extractFields(knownMembers, config)

        val classFields = fields.map { field =>
          val (args, resolve) =
            if (field.accessor)
              field.onType.memberType(field.method).widen.asType match {
                case '[t] =>
                  Expr(Nil) -> '{
                    (c: sangria.schema.Context[ctxType.Underlying, valType.Underlying]) =>
                      ${
                        ctxValType match {
                          case Some((tpe, fn)) =>
                            given Type[CtxVal] = tpe
                            wrappedInActionConversion[Ctx, t](
                              unsafeSelectByName[t]('{ $fn(c.ctx) }, field.method.name)
                            )
                          case None =>
                            wrappedInActionConversion[Ctx, t](
                              unsafeSelectByName[t]('{ c.value }, field.method.name)
                            )
                        }
                      }
                  }
              }
            else
              fieldWithArguments(config, field, ctxType, valType, ctxValType)

          @tailrec
          def methodResultType(typeRepr: TypeRepr): TypeRepr = typeRepr match
            case MethodType(_, _, retType) => methodResultType(retType)
            case retType => retType

          // Contextualize the method type with respect to the enclosing type
          val fieldType = methodResultType(field.onType.memberType(field.method).widen)
          val actualFieldType = findActualFieldType(fieldType)

          val annotationType = symbolOutputType(field.annotations)
          val graphQlType = annotationType.getOrElse {
            actualFieldType.asType match
              case '[t] =>
                Expr.summon[sangria.macros.derive.GraphQLOutputTypeLookup[t]] match
                  case Some(lookup) => '{ $lookup.graphqlType }
                  case None =>
                    reportSummoningErrors(
                      Seq(s"GraphQlOutputType not found: ${TypeRepr.of[t => Action[Ctx, _]].show}"),
                      Seq(None)
                    )
          }

          val name = field.name
          val annotationName = symbolName(field.annotations)
          val configName = config.collect { case MacroRenameField(`name`, expr, _) =>
            expr
          }.lastOption

          val annotationDescr = symbolDescription(field.annotations)
          val configDescr = config.collect { case MacroDocumentField(`name`, expr, _, _) =>
            expr
          }.lastOption

          val annotationDepr = flattenOptionExpr[String](symbolDeprecation(field.annotations))
          val configDocDepr = config
            .collect { case MacroDocumentField(`name`, _, reason, _) => reason }
            .lastOption
            .getOrElse(Expr(None))
          val configDepr = flattenOptionExpr[String](config.collect {
            case MacroDeprecateField(`name`, reason, _) => reason
          }.lastOption)

          val complexity = config.collect { case MacroFieldComplexity(`name`, c, _) =>
            c
          }.lastOption

          val annotationTags = symbolFieldTags(field.annotations)

          val configTags = config.foldLeft('{ List[sangria.execution.FieldTag]() }) {
            case (acc, MacroFieldTags(`name`, expr, _)) =>
              '{ $acc ++ ${ Expr.ofList(expr.toList) } }
            case (acc, _) => acc
          }

          val fieldName = {
            val nonTransformedName = configName.orElse(annotationName).getOrElse(Expr(name))

            config.collect { case MacroTransformFieldNames(fnt) => fnt }.lastOption match {
              case Some(fnt) => '{ $fnt($nonTransformedName) }
              case None => nonTransformedName
            }
          }

          actualFieldType.asType match
            case '[t] =>
              '{
                sangria.schema.Field[ctxType.Underlying, valType.Underlying, Any, Any](
                  $fieldName,
                  ${ graphQlType.asExprOf[OutputType[t]] },
                  ${ flattenOptionExpr[String](configDescr.orElse(annotationDescr)) },
                  $args,
                  ${ resolve },
                  Nil,
                  $configTags ++ $annotationTags,
                  ${
                    flattenOptionExpr[(ctxType.Underlying, sangria.schema.Args, Double) => Double](
                      complexity.asInstanceOf[Option[
                        quoted.Expr[(ctxType.Underlying, sangria.schema.Args, Double) => Double]]])
                  },
                  $configDocDepr.orElse($configDepr).orElse($annotationDepr)
                )
              }
        }

        val allFields = classFields ++ additionalFields(config)

        if (allFields.nonEmpty) Right(allFields.asInstanceOf[List[Expr[Field[Ctx, Val]]]])
        else Left(List(PositionByQuotes(globalQuotes) -> s"$targetType: Field list is empty"))
      case errors => Left(errors)
    }
  }

  private def findActualFieldType[T](fieldType: globalQuotes.reflect.TypeRepr) =
    import globalQuotes.reflect._
    if (isSupertype[Future[_]](fieldType) && fieldType.typeArgs.nonEmpty)
      fieldType.typeArgs.head
    else if (isSupertype[scala.util.Try[_]](fieldType) && fieldType.typeArgs.nonEmpty)
      fieldType.typeArgs.head
    else if (isSupertype[Deferred[_]](fieldType) && fieldType
        .baseType(TypeRepr.of[Deferred[_]].typeSymbol)
        .typeArgs
        .nonEmpty)
      fieldType.baseType(TypeRepr.of[Deferred[_]].typeSymbol).typeArgs.head
    else if (isSupertype[Action[_, _]](fieldType) && fieldType
        .baseType(TypeRepr.of[Action[_, _]].typeSymbol)
        .typeArgs
        .size == 2)
      fieldType.baseType(TypeRepr.of[Action[_, _]].typeSymbol).typeArgs(1)
    else
      fieldType

  private def isSupertype[T](using Quotes, Type[T])(subtype: quotes.reflect.TypeRepr) =
    import quotes.reflect._
    subtype <:< TypeRepr.of[T]

  private def fieldWithArguments[Ctx, CtxVal, Val](
      config: Seq[MacroDeriveObjectSetting],
      member: KnownMember,
      ctxType: Type[Ctx],
      valType: Type[Val],
      ctxValType: Option[(Type[CtxVal], Expr[Ctx => CtxVal])]) = {
    import globalQuotes.reflect._
    given Type[Val] = valType
    given Type[Ctx] = ctxType
    val args = member.method.paramSymss.map(_.map(arg => createArg(config, member)(arg)))
    def argsAst(using q: Quotes)(c: Expr[Context[Ctx, Val]]) = {
      import q.reflect._
      args.map(_.map {
        case NormalArg(name, tpe, _, false) =>
          tpe.asType match
            case '[t] => '{ $c.arg[t](${ Expr(name) }) }
        case NormalArg(name, tpe, _, true) =>
          tpe.asType match
            case '[t] => '{ $c.argOpt[t](${ Expr(name) }) }
        case ContextArg => c
      })
    }
    Expr.ofList(args.flatten.collect { case na: NormalArg => na.expr }) ->
      '{ (c: sangria.schema.Context[ctxType.Underlying, valType.Underlying]) =>
        ${
          import quotes.reflect._
          val expr = ctxValType match {
            case Some(tpe, fn) =>
              given Type[CtxVal] = tpe
              '{ $fn(c.ctx) }
            case None =>
              '{ c.value }
          }
          val method = Select.unique(expr.asTerm, member.method.name)
          val args = argsAst('c).map(_.map(_.asTerm))
          if (member.method.isDefDef)
            member.method.tree match
              case d: DefDef =>
                d.returnTpt.tpe.asType match
                  case '[t] =>
                    wrappedInActionConversion[Ctx, t](
                      args
                        .foldLeft[Select | Apply](method) { (m, argList) =>
                          Apply(m, argList)
                        }
                        .asExprOf[t]
                    )
          else
            globalQuotes.reflect.Ref(member.method).tpe.widen.asType match {
              case '[t] =>
                wrappedInActionConversion[Ctx, t](method.asExprOf[t])
            }
        }
      }
  }

  private def wrappedInActionConversion[Ctx: Type, T: Type](value: Expr[T]) = {
    import globalQuotes.reflect._
    Expr.summon[T => Action[Ctx, _]] match
      case Some(conversion) =>
        '{ $conversion(${ value }) }
      case None =>
        reportSummoningErrors(
          Seq(s"Implicit conversion not found: ${TypeRepr.of[T => Action[Ctx, _]].show}"),
          Seq(None)
        )
  }

  private def createArg(config: Seq[MacroDeriveObjectSetting], member: KnownMember)(
      arg: globalQuotes.reflect.Symbol) =
    import globalQuotes.reflect._
    arg match {
      case s: Symbol if Ref(s).tpe.widen.dealias <:< TypeRepr.of[Context[_, _]].widen =>
        ContextArg
      case s: Symbol =>
        val tpe = Ref(s).tpe.widen
        val methodName = member.method.name
        val argName = s.name

        val name = collectArgRename(config, methodName, argName)
          .orElse(symbolName(s.annotations).collect { case '{ $s } => s.valueOrAbort })
          .getOrElse(argName)

        val description = collectArgDescription(config, methodName, argName).orElse(
          symbolDescription(s.annotations))
        val default: Option[(TypeRepr, Expr[Any])] =
          collectArgDefault(config, methodName, argName).orElse(symbolDefault(s.annotations))

        val fieldType = tpe.asType match
          case '[t] =>
            symbolInputType(s.annotations).getOrElse(
              Expr.summon[sangria.macros.derive.GraphQLInputTypeLookup[t, _]] match
                case Some(lookup) => '{ $lookup.graphqlType }
                case None =>
                  reportSummoningErrors(
                    Seq(s"GraphQlInputType not found: ${tpe.show}"),
                    Seq(None)
                  )
            )

        val expr = {
          val inputType =
            fieldType.asTerm.tpe.baseType(TypeRepr.of[InputType].typeSymbol).widen.typeArgs.head
          inputType.asType match {
            case '[i] =>
              default match {
                case Some((defaultTpe, defaultValue)) =>
                  defaultTpe.asType match
                    case '[d] =>
                      (
                        Expr.summon[ToInput[d, _]],
                        Expr.summon[FromInput[Option[i]]],
                        Expr.summon[ArgumentType[Option[i]]]) match
                        case (Some(toInput), Some(fromInput), Some(argType)) =>
                          '{
                            sangria.schema.Argument.createWithDefault(
                              ${ Expr(name) },
                              sangria.schema.OptionInputType(${ fieldType.asExprOf[InputType[i]] }),
                              ${ flattenOptionExpr[String](description) },
                              ${ defaultValue.asExprOf[d] }
                            )($toInput, ${ fromInput }, ${ argType })
                          }
                        case optionTuple =>
                          reportSummoningErrors(
                            Seq(
                              s"GraphQlInputType not found: ${TypeRepr.of[ToInput[d, _]].show}",
                              s"GraphQlInputType not found: ${TypeRepr.of[FromInput[Option[i]]].show}",
                              s"GraphQlInputType not found: ${TypeRepr.of[ArgumentType[Option[i]]].show}"
                            ),
                            optionTuple.toList
                          )
                case None =>
                  (Expr.summon[FromInput[i]], Expr.summon[ArgumentType[i]]) match
                    case (Some(fromInput), Some(argType)) =>
                      '{
                        sangria.schema.Argument.createWithoutDefault(
                          ${ Expr(name) },
                          ${ fieldType.asExprOf[InputType[i]] },
                          ${ flattenOptionExpr[String](description) })($fromInput, $argType)
                      }
                    case optionTuple =>
                      reportSummoningErrors(
                        Seq(
                          s"GraphQlInputType not found: ${TypeRepr.of[FromInput[i]].show}",
                          s"GraphQlInputType not found: ${TypeRepr.of[ArgumentType[i]].show}"
                        ),
                        optionTuple.toList
                      )
              }
          }
        }

        val optional = default.isEmpty && isSupertype[Option[_]](tpe)

        val targetType =
          if (optional)
            tpe.baseType(TypeRepr.of[Option[_]].typeSymbol).typeArgs.head
          else
            tpe

        NormalArg(name, targetType, expr, optional)
    }

  private def findKnownMembers[T](tpe: Type[T], includeMethods: Set[String]): List[KnownMember] =
    import globalQuotes.reflect._
    given Type[T] = tpe
    val sym = TypeRepr.of[T].typeSymbol
    (sym.fieldMembers ++ sym.methodMembers).flatMap { m =>
      val flags = m.flags
      if (flags.is(Flags.CaseAccessor)) {
        Some(
          KnownMember(TypeRepr.of[T], m, findCaseClassAccessorAnnotations(tpe, m), accessor = true))
      } else if (flags.is(Flags.Method) && (memberField(m.annotations) || includeMethods.contains(
          m.name))) {
        Some(KnownMember(TypeRepr.of[T], m, m.annotations, accessor = false))
      } else if (m.isValDef && (memberField(m.annotations) || includeMethods.contains(m.name))) {
        Some(KnownMember(TypeRepr.of[T], m, m.annotations, accessor = false))
      } else None
    }.toList

  private def findCaseClassAccessorAnnotations[T](using
      Quotes)(tpe: Type[T], member: quotes.reflect.Symbol): List[quotes.reflect.Term] =
    import quotes.reflect._
    given Type[T] = tpe
    if (TypeRepr.of[T].widen.typeSymbol.companionModule == Symbol.noSymbol) Nil
    else {
      val annotationsConstructors =
        for {
          c <- TypeRepr.of[T].widen.typeSymbol.primaryConstructor.paramSymss
          p <- c
          if p.name == member.name
        } yield p.annotations
      annotationsConstructors.toList.flatten
    }

  private def extractFields(
      knownMembers: List[KnownMember],
      config: Seq[MacroDeriveObjectSetting]) = {
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

    knownMembers.filter { m =>
      actualFields.contains(m.name) && !memberExcluded(m.annotations)
    }
  }

  private def validateFieldConfig(
      knownMembers: List[KnownMember],
      config: Seq[MacroDeriveObjectSetting]) = {
    val knownMembersSet = knownMembers.map(_.name).toSet

    def unknownMember(pos: PositionPointer, name: String) =
      pos -> s"Unknown member '$name'. Known members are: ${knownMembers.map(_.name).mkString(", ")}"

    def getMethod(
        pos: PositionPointer,
        name: String): Either[List[(PositionPointer, String)], globalQuotes.reflect.Symbol] =
      knownMembers.withFilter(_.name == name).map(_.method) match {
        case method :: Nil => Right(method)
        case Nil => Left(unknownMember(pos, name) :: Nil)
        case _ =>
          Left(
            List(
              pos -> s"Cannot configure overloaded method '$name' using `DeriveObjectSetting` due to ambiguity, use annotations instead."
            ))
      }

    def getArgument(using Quotes)(
        pos: PositionPointer,
        methodName: String,
        argName: String): Either[List[(PositionPointer, String)], globalQuotes.reflect.Symbol] =
      getMethod(pos, methodName).flatMap { method =>
        val knownArguments = method.paramSymss.flatten
        knownArguments
          .find(_.name == argName)
          .map(Right(_))
          .getOrElse(
            Left(
              List(
                pos -> s"Unknown argument '$argName' of method '$method'. Known arguments are: ${knownArguments
                    .map(_.name)
                    .mkString(", ")}"
              )))
      }

    def validateHasArgument(pos: PositionPointer, methodName: String, argName: String) =
      getArgument(pos, methodName, argName).map(_ => Nil).merge

    config.toList.flatMap {
      case MacroIncludeFields(fields, pos) if !fields.forall(knownMembersSet.contains) =>
        val unknown = fields.diff(knownMembersSet)

        unknown.toList.map(unknownMember(pos, _))

      case MacroExcludeFields(fields, pos) if !fields.forall(knownMembersSet.contains) =>
        val unknown = fields.diff(knownMembersSet)

        unknown.toList.map(unknownMember(pos, _))

      case MacroDocumentField(fieldName, _, _, pos) if !knownMembersSet.contains(fieldName) =>
        unknownMember(pos, fieldName) :: Nil

      case MacroRenameField(fieldName, _, pos) if !knownMembersSet.contains(fieldName) =>
        unknownMember(pos, fieldName) :: Nil

      case MacroFieldTags(fieldName, _, pos) if !knownMembersSet.contains(fieldName) =>
        unknownMember(pos, fieldName) :: Nil

      case MacroDeprecateField(fieldName, _, pos) if !knownMembersSet.contains(fieldName) =>
        unknownMember(pos, fieldName) :: Nil

      case MacroFieldComplexity(fieldName, _, pos) if !knownMembersSet.contains(fieldName) =>
        unknownMember(pos, fieldName) :: Nil

      case MacroReplaceField(fieldName, _, pos) if !knownMembersSet.contains(fieldName) =>
        unknownMember(pos, fieldName) :: Nil

      case MacroMethodArgumentRename(methodName, argName, _, pos) =>
        validateHasArgument(pos, methodName, argName)

      case MacroMethodArgumentDescription(methodName, argName, _, pos) =>
        validateHasArgument(pos, methodName, argName)

      case MacroMethodArgumentsDescription(methodName, descriptions, pos) =>
        descriptions.keys.toList.flatMap(validateHasArgument(pos, methodName, _))

      case MacroMethodArgumentDefault(methodName, argName, _, _, pos) =>
        validateHasArgument(pos, methodName, argName)

      case MacroMethodArgument(methodName, argName, _, _, _, pos) =>
        validateHasArgument(pos, methodName, argName)

      case _ => Nil
    }
  }

  private def additionalFields[Ctx, Val](config: Seq[MacroDeriveObjectSetting]) =
    config.foldLeft(List[Expr[Field[_, _]]]()) {
      case (acc, MacroAddFields(fields)) => acc ++ fields
      case (acc, MacroReplaceField(_, field, _)) => acc :+ field
      case (acc, _) => acc
    }

  private def validateObjectConfig[T, Ctx, Val](using
      Quotes)(config: Seq[Expr[DeriveObjectSetting[Ctx, Val]]], tpe: Type[T]) = config.map {
    case '{ ObjectTypeName.apply[t, s]($name) } =>
      Right(MacroName(name))

    case '{ ObjectTypeDescription.apply[t, s]($description) } =>
      Right(MacroDescription(description))

    case '{ Interfaces.apply[t, s](${ Varargs(ints) }: _*) } =>
      Right(MacroInterfaces[t, s](ints))

    case expr @ '{ DocumentField.apply[t, s]($fieldName, $description, $deprecationReason) } =>
      Right(
        MacroDocumentField(
          fieldName.valueOrAbort,
          description,
          deprecationReason,
          PositionByExpr(expr)))

    case expr @ '{ RenameField.apply[t, s]($fieldName, $graphqlName) } =>
      Right(MacroRenameField(fieldName.valueOrAbort, graphqlName, PositionByExpr(expr)))

    case expr @ '{ FieldTags.apply[t, s]($fieldName, ${ Varargs(fieldTags) }: _*) } =>
      Right(MacroFieldTags(fieldName.valueOrAbort, fieldTags, PositionByExpr(expr)))

    case expr @ '{ DeprecateField.apply[t, s]($fieldName, $deprecationReason) } =>
      Right(MacroDeprecateField(fieldName.valueOrAbort, deprecationReason, PositionByExpr(expr)))

    case expr @ '{ FieldComplexity.apply[t, s]($fieldName, $complexity) } =>
      Right(MacroFieldComplexity[t](fieldName.valueOrAbort, complexity, PositionByExpr(expr)))

    case expr @ '{ IncludeFields.apply[t, s](${ Varargs(fields) }: _*) } =>
      Right(MacroIncludeFields(fields.map(_.valueOrAbort).toSet, PositionByExpr(expr)))

    case expr @ '{ IncludeMethods.apply[t, s](${ Varargs(methods) }: _*) } =>
      import quotes.reflect._
      given Type[T] = tpe
      val typeSymbol = TypeRepr.of[T].typeSymbol
      val known = (typeSymbol.methodMembers ++ typeSymbol.fieldMembers)
        .filter { symbol =>
          val flags = symbol.flags
          symbol.isTerm && flags.is(Flags.Method)
        }
        .map(_.name)
        .toSet
      val unknown = methods.map(_.valueOrAbort).filterNot(known.contains)

      if (unknown.isEmpty) Right(MacroIncludeMethods(methods.map(_.valueOrAbort).toSet))
      else
        Left(PositionByExpr(
          expr) -> s"Unknown members: ${unknown.mkString(", ")}. Known members are: ${known.mkString(", ")}")

    case expr @ '{ ExcludeFields.apply[t, s](${ Varargs(fields) }: _*) } =>
      Right(MacroExcludeFields(fields.map(_.valueOrAbort).toSet, PositionByExpr(expr)))

    case '{ AddFields.apply[t, s](${ Varargs(fields) }: _*) } =>
      Right(MacroAddFields(fields.toList))

    case expr @ '{ ReplaceField.apply[t, s]($fieldName, $field) } =>
      Right(MacroReplaceField(fieldName.valueOrAbort, field, PositionByExpr(expr)))

    case '{ TransformFieldNames.apply[t, s]($fn) } =>
      Right(MacroTransformFieldNames(fn))

    case expr @ '{ MethodArgumentRename.apply[t, s]($methodName, $argName, $newName) } =>
      Right(
        MacroMethodArgumentRename(
          methodName.valueOrAbort,
          argName.valueOrAbort,
          newName.valueOrAbort,
          PositionByExpr(expr)))

    case expr @ '{ MethodArgumentDescription.apply[t, s]($methodName, $argName, $description) } =>
      Right(
        MacroMethodArgumentDescription(
          methodName.valueOrAbort,
          argName.valueOrAbort,
          description,
          PositionByExpr(expr)))

    case expr @ '{
          MethodArgumentsDescription.apply[t, s]($methodName, ${ Varargs(descriptions) }: _*)
        } =>
      val descriptionsMap = descriptions.map {
        case '{ scala.Predef.ArrowAssoc[String](${ argName }: String).->[String]($description) } =>
          argName.valueOrAbort -> description
      }.toMap
      Right(
        MacroMethodArgumentsDescription(
          methodName.valueOrAbort,
          descriptionsMap,
          PositionByExpr(expr)))

    case expr @ '{
          MethodArgumentDefault.apply[t, s, arg]($methodName, $argName, $default)
        } =>
      Right(
        MacroMethodArgumentDefault(
          methodName.valueOrAbort,
          argName.valueOrAbort,
          globalQuotes.reflect.TypeRepr.of[arg],
          default,
          PositionByExpr(expr)))

    case expr @ '{
          MethodArgument.apply[t, s, arg]($methodName, $argName, $description, $default)
        } =>
      Right(
        MacroMethodArgument(
          methodName.valueOrAbort,
          argName.valueOrAbort,
          description,
          globalQuotes.reflect.TypeRepr.of[arg],
          default,
          PositionByExpr(expr)))

    case expr =>
      Left(
        PositionByExpr(expr) ->
          "Unsupported shape of derivation config. Please define subclasses of `DeriveObjectTypeSetting` directly in the argument list of the macro.")
  }

  private case class KnownMember(
      onType: globalQuotes.reflect.TypeRepr,
      method: globalQuotes.reflect.Symbol,
      annotations: List[globalQuotes.reflect.Term],
      accessor: Boolean) {
    lazy val name = method.name.toString
  }

  private sealed trait Arg

  private case object ContextArg extends Arg
  private case class NormalArg(
      name: String,
      tpe: globalQuotes.reflect.TypeRepr,
      expr: Expr[sangria.schema.Argument[?]],
      optional: Boolean)
      extends Arg

  sealed trait MacroDeriveObjectSetting

  private case class MacroName(name: Expr[String]) extends MacroDeriveObjectSetting
  private case class MacroDescription(description: Expr[String]) extends MacroDeriveObjectSetting
  private case class MacroInterfaces[Ctx, Val](interfaces: Seq[Expr[PossibleInterface[Ctx, Val]]])
      extends MacroDeriveObjectSetting

  private case class MacroDocumentField(
      fieldName: String,
      description: Expr[String],
      deprecationReason: Expr[Option[String]],
      pos: PositionPointer)
      extends MacroDeriveObjectSetting
  private case class MacroRenameField(
      fieldName: String,
      graphqlName: Expr[String],
      pos: PositionPointer)
      extends MacroDeriveObjectSetting
  private case class MacroFieldTags(
      fieldName: String,
      tags: Seq[Expr[FieldTag]],
      pos: PositionPointer)
      extends MacroDeriveObjectSetting
  private case class MacroDeprecateField(
      fieldName: String,
      deprecationReason: Expr[String],
      pos: PositionPointer)
      extends MacroDeriveObjectSetting
  private case class MacroFieldComplexity[Ctx](
      fieldName: String,
      complexity: Expr[(Ctx, sangria.schema.Args, Double) => Double],
      pos: PositionPointer)
      extends MacroDeriveObjectSetting

  private case class MacroIncludeFields(fieldNames: Set[String], pos: PositionPointer)
      extends MacroDeriveObjectSetting
  private case class MacroIncludeMethods(methodNames: Set[String]) extends MacroDeriveObjectSetting
  private case class MacroExcludeFields(fieldNames: Set[String], pos: PositionPointer)
      extends MacroDeriveObjectSetting
  private case class MacroAddFields[Ctx, Val](fields: List[Expr[Field[Ctx, Val]]])
      extends MacroDeriveObjectSetting
  private case class MacroReplaceField[Ctx, Val](
      fieldName: String,
      field: Expr[Field[Ctx, Val]],
      pos: PositionPointer)
      extends MacroDeriveObjectSetting
  private case class MacroTransformFieldNames(transformer: Expr[String => String])
      extends MacroDeriveObjectSetting

  private case class MacroMethodArgumentRename(
      methodName: String,
      argName: String,
      newName: String,
      pos: PositionPointer)
      extends MacroDeriveObjectSetting
  private case class MacroMethodArgumentDescription(
      methodName: String,
      argName: String,
      description: Expr[String],
      pos: PositionPointer)
      extends MacroDeriveObjectSetting
  private case class MacroMethodArgumentsDescription(
      methodName: String,
      descriptions: Map[String, Expr[String]],
      pos: PositionPointer)
      extends MacroDeriveObjectSetting
  private case class MacroMethodArgumentDefault[Arg](
      methodName: String,
      argName: String,
      defaultType: globalQuotes.reflect.TypeRepr,
      default: Expr[Arg],
      pos: PositionPointer)
      extends MacroDeriveObjectSetting
  private case class MacroMethodArgument[Arg](
      methodName: String,
      argName: String,
      description: Expr[String],
      defaultType: globalQuotes.reflect.TypeRepr,
      default: Expr[Arg],
      pos: PositionPointer)
      extends MacroDeriveObjectSetting

  private def collectArgRename(
      config: Seq[MacroDeriveObjectSetting],
      methodName: String,
      argName: String) = config.collect {
    case MacroMethodArgumentRename(`methodName`, `argName`, newName, _) => newName
  }.lastOption

  private def collectArgDescription(
      config: Seq[MacroDeriveObjectSetting],
      methodName: String,
      argName: String) = config
    .collect {
      case MacroMethodArgumentDescription(`methodName`, `argName`, description, _) =>
        Some(description)
      case MacroMethodArgumentsDescription(`methodName`, descriptions, _) =>
        descriptions.get(argName)
      case MacroMethodArgument(`methodName`, `argName`, description, _, _, _) => Some(description)
    }
    .flatten
    .lastOption

  private def collectArgDefault(
      config: Seq[MacroDeriveObjectSetting],
      methodName: String,
      argName: String): Option[(globalQuotes.reflect.TypeRepr, Expr[Any])] = config.collect {
    case MacroMethodArgumentDefault(`methodName`, `argName`, defaultTpe, default, _) =>
      (defaultTpe, default)
    case MacroMethodArgument(`methodName`, `argName`, _, defaultTpe, default, _) =>
      (defaultTpe, default)
  }.lastOption
}
