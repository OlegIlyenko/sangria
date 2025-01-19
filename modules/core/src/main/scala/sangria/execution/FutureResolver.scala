package sangria.execution

import sangria.ast
import sangria.ast.{AstLocation, Document, SourceMapper}
import sangria.execution.deferred.{Deferred, DeferredResolver}
import sangria.marshalling.ResultMarshaller
import sangria.schema._
import sangria.streaming.SubscriptionStream

import scala.annotation.tailrec
import scala.collection.immutable.{ListMap, VectorBuilder}
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

private[execution] object FutureResolverBuilder extends ResolverBuilder {
  override def build[Ctx](
      marshaller: ResultMarshaller,
      middlewareCtx: MiddlewareQueryContext[Ctx, _, _],
      schema: Schema[Ctx, _],
      valueCollector: ValueCollector[Ctx, _],
      variables: Map[String, VariableValue],
      fieldCollector: FieldCollector[Ctx, _],
      userContext: Ctx,
      exceptionHandler: ExceptionHandler,
      deferredResolver: DeferredResolver[Ctx],
      sourceMapper: Option[SourceMapper],
      deprecationTracker: Option[DeprecationTracker],
      middleware: List[(Any, Middleware[Ctx])],
      maxQueryDepth: Option[Int],
      deferredResolverState: Any,
      preserveOriginalErrors: Boolean,
      validationTiming: TimeMeasurement,
      queryReducerTiming: TimeMeasurement,
      queryAst: Document)(implicit executionContext: ExecutionContext): Resolver[Ctx] =
    new FutureResolver[Ctx](
      marshaller,
      middlewareCtx,
      schema,
      valueCollector,
      variables,
      fieldCollector,
      userContext,
      exceptionHandler,
      deferredResolver,
      sourceMapper,
      deprecationTracker,
      middleware,
      maxQueryDepth,
      deferredResolverState,
      preserveOriginalErrors,
      validationTiming,
      queryReducerTiming,
      queryAst
    )
}

/** [[Resolver]] using [[scala.concurrent.Future]] and [[scala.concurrent.Promise]] as base
  * asynchronous primitives
  */
private[execution] class FutureResolver[Ctx](
    val marshaller: ResultMarshaller,
    middlewareCtx: MiddlewareQueryContext[Ctx, _, _],
    schema: Schema[Ctx, _],
    valueCollector: ValueCollector[Ctx, _],
    variables: Map[String, VariableValue],
    fieldCollector: FieldCollector[Ctx, _],
    userContext: Ctx,
    exceptionHandler: ExceptionHandler,
    deferredResolver: DeferredResolver[Ctx],
    sourceMapper: Option[SourceMapper],
    deprecationTracker: Option[DeprecationTracker],
    middleware: List[(Any, Middleware[Ctx])],
    maxQueryDepth: Option[Int],
    deferredResolverState: Any,
    preserveOriginalErrors: Boolean,
    validationTiming: TimeMeasurement,
    queryReducerTiming: TimeMeasurement,
    queryAst: ast.Document
)(implicit executionContext: ExecutionContext)
    extends Resolver[Ctx] {
  private val resultResolver =
    new ResultResolver(marshaller, exceptionHandler, preserveOriginalErrors)
  private val toScalarMiddleware =
    Middleware.composeToScalarMiddleware(middleware.map(_._2), userContext)

  import Resolver._
  import resultResolver._

  override def resolveFieldsPar(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] = {
    val actions =
      collectActionsPar(ExecutionPath.empty, tpe, value, fields, ErrorRegistry.empty, userContext)

    handleScheme(
      processFinalResolve(
        resolveActionsPar(ExecutionPath.empty, tpe, actions, userContext, fields.namesOrdered))
        .map(_ -> userContext),
      scheme)
  }

  override def resolveFieldsSeq(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] = {
    val result = resolveSeq(ExecutionPath.empty, tpe, value, fields)

    handleScheme(result.flatMap(res => processFinalResolve(res._1).map(_ -> res._2)), scheme)
  }

  override def resolveFieldsSubs(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] =
    scheme match {
      case ExecutionScheme.Default =>
        val (s, res) = resolveSubs[({ type X[_] })#X](
          ExecutionPath.empty,
          tpe,
          value,
          fields,
          ErrorRegistry.empty,
          None)

        s.first(res).map(_._2).asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

      case ExecutionScheme.Extended =>
        val (s, res) = resolveSubs[({ type X[_] })#X](
          ExecutionPath.empty,
          tpe,
          value,
          fields,
          ErrorRegistry.empty,
          None)

        s.first(res)
          .map { case (errors, res) =>
            ExecutionResult(
              userContext,
              res,
              errors,
              middleware,
              validationTiming,
              queryReducerTiming)
          }
          .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

      case es: ExecutionScheme.StreamBasedExecutionScheme[
            ({ type X [_] })#X @unchecked] @unchecked =>
        val (_, res) = resolveSubs(
          ExecutionPath.empty,
          tpe,
          value,
          fields,
          ErrorRegistry.empty,
          Some(es.subscriptionStream))

        es.subscriptionStream
          .map(res) {
            case (errors, r) if es.extended =>
              ExecutionResult(
                userContext,
                r,
                errors,
                middleware,
                validationTiming,
                queryReducerTiming)
            case (_, r) => r
          }
          .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

      case s =>
        throw new IllegalStateException(s"Unsupported execution scheme: $s")
    }

  protected def handleScheme(
      result: Future[((Vector[RegisteredError], marshaller.Node), Ctx)],
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] = scheme match {
    case ExecutionScheme.Default =>
      result.map { case ((_, res), _) => res }.asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

    case ExecutionScheme.Extended =>
      result
        .map { case ((errors, res), uc) =>
          ExecutionResult(uc, res, errors, middleware, validationTiming, queryReducerTiming)
        }
        .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

    case s: ExecutionScheme.StreamBasedExecutionScheme[_] @unchecked =>
      s.subscriptionStream
        .singleFuture(result.map {
          case ((errors, res), uc) if s.extended =>
            ExecutionResult(uc, res, errors, middleware, validationTiming, queryReducerTiming)
          case ((_, res), _) => res
        })
        .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

    case s =>
      throw new IllegalStateException(s"Unsupported execution scheme: $s")
  }

  private def processFinalResolve(resolve: Resolve) = resolve match {
    case Result(errors, data, _) =>
      Future.successful(
        errors.originalErrors ->
          marshalResult(
            data.asInstanceOf[Option[resultResolver.marshaller.Node]],
            marshalErrors(errors),
            marshallExtensions.asInstanceOf[Option[resultResolver.marshaller.Node]],
            beforeExecution = false
          ).asInstanceOf[marshaller.Node])

    case dr: DeferredResult =>
      immediatelyResolveDeferred(
        userContext,
        dr,
        _.map { case Result(errors, data, _) =>
          errors.originalErrors ->
            marshalResult(
              data.asInstanceOf[Option[resultResolver.marshaller.Node]],
              marshalErrors(errors),
              marshallExtensions.asInstanceOf[Option[resultResolver.marshaller.Node]],
              beforeExecution = false
            ).asInstanceOf[marshaller.Node]
        }
      )
  }

  private def marshallExtensions: Option[marshaller.Node] = {
    val extensions =
      middleware.flatMap {
        case (v, m: MiddlewareExtension[Ctx @unchecked]) =>
          m.afterQueryExtensions(v.asInstanceOf[m.QueryVal], middlewareCtx)
        case _ => Nil
      }

    if (extensions.nonEmpty) ResultResolver.marshalExtensions(marshaller, extensions)
    else None
  }

  private def immediatelyResolveDeferred[T](
      uc: Ctx,
      dr: DeferredResult,
      fn: Future[Result] => Future[T]): Future[T] = {
    val res = fn(dr.futureValue)

    resolveDeferredWithGrouping(dr.deferred).foreach(groups =>
      groups.foreach(group => resolveDeferred(uc, group)))

    res
  }

  private def resolveDeferredWithGrouping(deferred: Vector[Future[Vector[Defer]]]) =
    Future.sequence(deferred).map(listOfDef => deferredResolver.groupDeferred(listOfDef.flatten))

  private type Actions = (
      ErrorRegistry,
      Option[Vector[(
          Vector[ast.Field],
          Option[(Field[Ctx, _], Option[MappedCtxUpdate[Ctx, Any, Any]], LeafAction[Ctx, _])])]])

  private def resolveSubs[S[_]](
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      value: Any,
      fields: CollectedFields,
      errorReg: ErrorRegistry,
      requestedStream: Option[SubscriptionStream[S]])
      : (SubscriptionStream[S], S[(Vector[RegisteredError], marshaller.Node)]) = {
    val firstStream = tpe.uniqueFields.head.tags
      .collectFirst { case SubscriptionField(s) => s }
      .get
      .asInstanceOf[SubscriptionStream[S]]
    val stream = requestedStream.fold(firstStream) { s =>
      if (s.supported(firstStream)) s
      else
        throw new IllegalStateException(
          "Subscription type field stream implementation is incompatible with requested stream implementation")
    }

    def marshallResult(result: Result): Any =
      stream.single(result)

    val fieldStreams = fields.fields.flatMap {
      case CollectedField(_, origField, _) if tpe.getField(schema, origField.name).isEmpty =>
        None
      case CollectedField(_, origField, Failure(error)) =>
        val resMap = marshaller.emptyMapNode(Seq(origField.outputName))

        Some(
          marshallResult(Result(
            errorReg.add(path.add(origField, tpe), error),
            if (isOptional(tpe, origField.name))
              Some(marshaller
                .addMapNodeElem(resMap, origField.outputName, marshaller.nullNode, optional = true))
            else None
          )))
      case CollectedField(_, origField, Success(fields)) =>
        resolveField(
          userContext,
          tpe,
          path.add(origField, tpe),
          value,
          ErrorRegistry.empty,
          fields) match {
          case _: StandardFieldResolution =>
            throw new IllegalStateException(
              "StandardFieldResolution is not supposed to appear here")
          case ErrorFieldResolution(updatedErrors) if isOptional(tpe, origField.name) =>
            val resMap = marshaller.emptyMapNode(Seq(origField.outputName))

            Some(
              marshallResult(
                Result(
                  updatedErrors,
                  Some(
                    marshaller.addMapNodeElem(
                      resMap,
                      fields.head.outputName,
                      marshaller.nullNode,
                      optional = isOptional(tpe, origField.name)))
                )))
          case ErrorFieldResolution(updatedErrors) =>
            Some(marshallResult(Result(updatedErrors, None)))
          case StreamFieldResolution(updatedErrors, svalue, standardFn) =>
            val s = svalue.stream.mapFuture[Any, Result](svalue.source) { action =>
              val res =
                Result(updatedErrors, Some(marshaller.emptyMapNode(Seq(origField.outputName))))
              val standardAction = standardFn(action)

              resolveStandardFieldResolutionSeq(
                path,
                userContext,
                tpe,
                origField,
                fields,
                res,
                standardAction)
                .map(_._1)
            }

            val recovered = svalue.stream.recover(s) { e =>
              val resMap = marshaller.emptyMapNode(Seq(origField.outputName))

              Result(
                updatedErrors.add(path.add(origField, tpe), e),
                if (isOptional(tpe, origField.name))
                  Some(
                    marshaller.addMapNodeElem(
                      resMap,
                      origField.outputName,
                      marshaller.nullNode,
                      optional = true))
                else None
              )
            }

            Some(recovered)
        }
    }

    stream -> stream.mapFuture(stream.merge(fieldStreams.asInstanceOf[Vector[S[Result]]]))(r =>
      processFinalResolve(r.buildValue))
  }

  private def resolveSeq(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      value: Any,
      fields: CollectedFields): Future[(Result, Ctx)] =
    fields.fields
      .foldLeft(
        Future.successful(
          (
            Result(ErrorRegistry.empty, Some(marshaller.emptyMapNode(fields.namesOrdered))),
            userContext))) { case (future, elem) =>
        future.flatMap { resAndCtx =>
          (resAndCtx, elem) match {
            case (acc @ (Result(_, None, _), _), _) => Future.successful(acc)
            case (acc, CollectedField(_, origField, _))
                if tpe.getField(schema, origField.name).isEmpty =>
              Future.successful(acc)
            case (
                  (Result(errors, Some(acc), _), uc),
                  CollectedField(_, origField, Failure(error))) =>
              Future.successful(Result(
                errors.add(path.add(origField, tpe), error),
                if (isOptional(tpe, origField.name))
                  Some(
                    marshaller.addMapNodeElem(
                      acc.asInstanceOf[marshaller.MapBuilder],
                      origField.outputName,
                      marshaller.nullNode,
                      optional = true))
                else None
              ) -> uc)
            case (
                  (accRes @ Result(errors, Some(acc), _), uc),
                  CollectedField(_, origField, Success(fields))) =>
              resolveSingleFieldSeq(path, uc, tpe, value, errors, origField, fields, accRes, acc)
          }
        }
      }
      .map { case (res, ctx) =>
        res.buildValue -> ctx
      }

  private def resolveSingleFieldSeq(
      path: ExecutionPath,
      uc: Ctx,
      tpe: ObjectType[Ctx, _],
      value: Any,
      errors: ErrorRegistry,
      origField: ast.Field,
      fields: Vector[ast.Field],
      accRes: Result,
      acc: Any // from `accRes`
  ): Future[(Result, Ctx)] =
    resolveField(uc, tpe, path.add(origField, tpe), value, errors, fields) match {
      case ErrorFieldResolution(updatedErrors) if isOptional(tpe, origField.name) =>
        Future.successful(
          Result(
            updatedErrors,
            Some(
              marshaller.addMapNodeElem(
                acc.asInstanceOf[marshaller.MapBuilder],
                fields.head.outputName,
                marshaller.nullNode,
                optional = isOptional(tpe, origField.name)))
          ) -> uc)
      case ErrorFieldResolution(updatedErrors) =>
        Future.successful(Result(updatedErrors, None) -> uc)
      case resolution: StandardFieldResolution =>
        resolveStandardFieldResolutionSeq(path, uc, tpe, origField, fields, accRes, resolution)
      case _: StreamFieldResolution[_, _] =>
        Future.failed(
          new IllegalStateException("StreamFieldResolution is not supposed to happen here"))
    }

  private def resolveStandardFieldResolutionSeq(
      path: ExecutionPath,
      uc: Ctx,
      tpe: ObjectType[Ctx, _],
      origField: ast.Field,
      fields: Vector[ast.Field],
      accRes: Result,
      resolution: StandardFieldResolution
  ): Future[(Result, Ctx)] = {
    val StandardFieldResolution(updatedErrors, result, newUc) = resolution
    val sfield = tpe.getField(schema, origField.name).head
    val fieldPath = path.add(fields.head, tpe)

    def resolveUc(v: Any) = newUc.fold(uc)(_.ctxFn(v))

    def resolveError(e: Throwable) = {
      try newUc.foreach(_.onError(e))
      catch {
        case NonFatal(ee) => ee.printStackTrace()
      }

      e
    }

    def resolveVal(v: Any) = newUc match {
      case Some(MappedCtxUpdate(_, mapFn, _)) => mapFn(v)
      case None => v
    }

    val resolve =
      try {
        result match {
          case Value(v) =>
            val updatedUc = resolveUc(v)

            Future.successful(
              resolveValue(
                fieldPath,
                fields,
                sfield.fieldType,
                sfield,
                resolveVal(v),
                updatedUc) -> updatedUc)

          case SequenceLeafAction(actions) =>
            val values = resolveActionSequenceValues(fieldPath, fields, sfield, actions)
            val future = Future.sequence(values.map(_.value))

            val resolved = future
              .flatMap { vs =>
                val errors = vs.iterator.flatMap(_.errors).toVector
                val successfulValues = vs.collect { case SeqFutRes(v, _, _) if v != null => v }
                val dctx = vs.collect { case SeqFutRes(_, _, d) if d != null => d }

                def resolveDctx(resolve: Resolve) = {
                  val last = dctx.lastOption
                  val init = if (dctx.isEmpty) dctx else dctx.init

                  resolve match {
                    case res: Result =>
                      dctx.foreach(_.promise.success(Vector.empty))
                      Future.successful(res)
                    case res: DeferredResult =>
                      init.foreach(_.promise.success(Vector.empty))
                      last.foreach(_.promise.success(res.deferred))
                      res.futureValue
                  }
                }

                errors.foreach(resolveError)

                if (successfulValues.size == vs.size)
                  resolveDctx(
                    resolveValue(
                      fieldPath,
                      fields,
                      sfield.fieldType,
                      sfield,
                      resolveVal(successfulValues),
                      resolveUc(successfulValues))
                      .appendErrors(fieldPath, errors, fields.head.location))
                else
                  resolveDctx(
                    Result(
                      ErrorRegistry.empty.append(fieldPath, errors, fields.head.location),
                      None))
              }
              .recover { case e =>
                Result(ErrorRegistry(fieldPath, resolveError(e), fields.head.location), None)
              }

            val deferred = values.iterator.collect {
              case SeqRes(_, d, _) if d != null => d
            }.toVector
            val deferredFut = values.iterator.collect {
              case SeqRes(_, _, d) if d != null => d
            }.toVector

            immediatelyResolveDeferred(
              uc,
              DeferredResult(Future.successful(deferred) +: deferredFut, resolved),
              _.map(r => r -> r.userContext.getOrElse(uc)))

          case PartialValue(v, es) =>
            val updatedUc = resolveUc(v)

            es.foreach(resolveError)

            Future.successful(
              resolveValue(fieldPath, fields, sfield.fieldType, sfield, resolveVal(v), updatedUc)
                .appendErrors(fieldPath, es, fields.head.location) -> updatedUc)
          case TryValue(v) =>
            Future.successful(v match {
              case Success(success) =>
                val updatedUc = resolveUc(success)

                resolveValue(
                  fieldPath,
                  fields,
                  sfield.fieldType,
                  sfield,
                  resolveVal(success),
                  updatedUc) -> updatedUc
              case Failure(e) =>
                Result(ErrorRegistry(fieldPath, resolveError(e), fields.head.location), None) -> uc
            })
          case DeferredValue(d) =>
            val p = Promise[(ChildDeferredContext, Any, Vector[Throwable])]()
            val (args, complexity) = calcComplexity(fieldPath, origField, sfield, userContext)
            val defer = Defer(p, d, complexity, sfield, fields, args)

            immediatelyResolveDeferred(
              uc,
              DeferredResult(
                Vector(Future.successful(Vector(defer))),
                p.future
                  .flatMap { case (dctx, v, es) =>
                    val updatedUc = resolveUc(v)

                    es.foreach(resolveError)

                    resolveValue(
                      fieldPath,
                      fields,
                      sfield.fieldType,
                      sfield,
                      resolveVal(v),
                      updatedUc).appendErrors(fieldPath, es, fields.head.location) match {
                      case r: Result => dctx.resolveResult(r.copy(userContext = Some(updatedUc)))
                      case er: DeferredResult =>
                        dctx
                          .resolveDeferredResult(er)
                          .map(_.copy(userContext = Some(updatedUc)))
                    }
                  }
                  .recover { case e =>
                    Result(ErrorRegistry(fieldPath, resolveError(e), fields.head.location), None)
                  }
              ),
              _.map(r => r -> r.userContext.getOrElse(uc))
            )
          case FutureValue(f) =>
            f.map { v =>
              val updatedUc = resolveUc(v)

              resolveValue(
                fieldPath,
                fields,
                sfield.fieldType,
                sfield,
                resolveVal(v),
                updatedUc) -> updatedUc
            }.recover { case e =>
              Result(
                ErrorRegistry(path.add(origField, tpe), resolveError(e), fields.head.location),
                None) -> uc
            }
          case PartialFutureValue(f) =>
            f.map { case PartialValue(v, es) =>
              val updatedUc = resolveUc(v)

              es.foreach(resolveError)

              resolveValue(fieldPath, fields, sfield.fieldType, sfield, resolveVal(v), updatedUc)
                .appendErrors(fieldPath, es, fields.head.location) -> updatedUc
            }.recover { case e =>
              Result(
                ErrorRegistry(path.add(origField, tpe), resolveError(e), fields.head.location),
                None) -> uc
            }
          case DeferredFutureValue(df) =>
            val p = Promise[(ChildDeferredContext, Any, Vector[Throwable])]()
            def defer(d: Deferred[Any]) = {
              val (args, complexity) = calcComplexity(fieldPath, origField, sfield, userContext)
              Defer(p, d, complexity, sfield, fields, args)
            }

            val actualDeferred = df
              .map(d => Vector(defer(d)))
              .recover { case NonFatal(e) =>
                p.failure(e)
                Vector.empty
              }

            immediatelyResolveDeferred(
              uc,
              DeferredResult(
                Vector(actualDeferred),
                p.future
                  .flatMap { case (dctx, v, es) =>
                    val updatedUc = resolveUc(v)

                    es.foreach(resolveError)

                    resolveValue(
                      fieldPath,
                      fields,
                      sfield.fieldType,
                      sfield,
                      resolveVal(v),
                      updatedUc).appendErrors(fieldPath, es, fields.head.location) match {
                      case r: Result => dctx.resolveResult(r.copy(userContext = Some(updatedUc)))
                      case er: DeferredResult =>
                        dctx
                          .resolveDeferredResult(er)
                          .map(_.copy(userContext = Some(updatedUc)))
                    }
                  }
                  .recover { case e =>
                    Result(ErrorRegistry(fieldPath, resolveError(e), fields.head.location), None)
                  }
              ),
              _.map(r => r -> r.userContext.getOrElse(uc))
            )
          case SubscriptionValue(_, _) =>
            Future.failed(
              new IllegalStateException(
                "Subscription values are not supported for normal operations"))
          case e =>
            Future.failed(
              new IllegalStateException(s"${e.getClass.toString} is not supposed to appear here"))
        }
      } catch {
        case NonFatal(e) =>
          Future.successful(
            Result(ErrorRegistry(fieldPath, resolveError(e), fields.head.location), None) -> uc)
      }

    resolve.flatMap {
      case (r: Result, newUc) =>
        Future.successful(
          accRes.addToMap(
            r,
            fields.head.outputName,
            isOptional(tpe, fields.head.name),
            fieldPath,
            fields.head.location,
            updatedErrors) -> newUc)
      case (dr: DeferredResult, newUc) =>
        immediatelyResolveDeferred(
          newUc,
          dr,
          _.map(
            accRes.addToMap(
              _,
              fields.head.outputName,
              isOptional(tpe, fields.head.name),
              fieldPath,
              fields.head.location,
              updatedErrors) -> newUc))
    }
  }

  private def calcComplexity(
      path: ExecutionPath,
      astField: ast.Field,
      field: Field[Ctx, _],
      uc: Ctx) = {
    val args = valueCollector.getFieldArgumentValues(
      path,
      Some(astField),
      field.arguments,
      astField.arguments,
      variables)

    args match {
      case Success(a) => a -> field.complexity.fold(DefaultComplexity)(_(uc, a, DefaultComplexity))
      case _ => Args.empty -> DefaultComplexity
    }
  }

  private def collectActionsPar(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      value: Any,
      fields: CollectedFields,
      errorReg: ErrorRegistry,
      userCtx: Ctx): Actions =
    fields.fields.foldLeft((errorReg, Some(Vector.empty)): Actions) {
      case (acc @ (_, None), _) => acc
      case (acc, CollectedField(_, origField, _)) if tpe.getField(schema, origField.name).isEmpty =>
        acc
      case ((errors, Some(acc)), CollectedField(_, origField, Failure(error))) =>
        errors.add(path.add(origField, tpe), error) -> (if (isOptional(tpe, origField.name))
                                                          Some(acc :+ (Vector(origField) -> None))
                                                        else None)
      case ((errors, Some(acc)), CollectedField(_, origField, Success(fields))) =>
        resolveField(userCtx, tpe, path.add(origField, tpe), value, errors, fields) match {
          case StandardFieldResolution(updatedErrors, result, updateCtx) =>
            updatedErrors -> Some(
              acc :+ (fields -> Some(
                (tpe.getField(schema, origField.name).head, updateCtx, result))))
          case ErrorFieldResolution(updatedErrors) if isOptional(tpe, origField.name) =>
            updatedErrors -> Some(acc :+ (Vector(origField) -> None))
          case ErrorFieldResolution(updatedErrors) => updatedErrors -> None
          case _: StreamFieldResolution[_, _] =>
            throw new IllegalStateException("IllegalStateException is not supposed to happen here")
        }
    }

  private def resolveActionSequenceValues(
      fieldsPath: ExecutionPath,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      actions: Seq[LeafAction[Any, Any]]): Seq[SeqRes] =
    actions.map {
      case Value(v) => SeqRes(SeqFutRes(v))
      case TryValue(Success(v)) => SeqRes(SeqFutRes(v))
      case TryValue(Failure(e)) => SeqRes(SeqFutRes(errors = Vector(e)))
      case PartialValue(v, es) => SeqRes(SeqFutRes(v, es))
      case FutureValue(future) =>
        SeqRes(future.map(v => SeqFutRes(v)).recover { case e => SeqFutRes(errors = Vector(e)) })
      case PartialFutureValue(future) =>
        SeqRes(future.map { case PartialValue(v, es) => SeqFutRes(v, es) }.recover { case e =>
          SeqFutRes(errors = Vector(e))
        })
      case DeferredValue(deferred) =>
        val promise = Promise[(ChildDeferredContext, Any, Vector[Throwable])]()
        val (args, complexity) = calcComplexity(fieldsPath, astFields.head, field, userContext)
        val defer = Defer(promise, deferred, complexity, field, astFields, args)

        SeqRes(
          promise.future.map { case (dctx, v, es) => SeqFutRes(v, es, dctx) }.recover { case e =>
            SeqFutRes(errors = Vector(e))
          },
          defer)
      case DeferredFutureValue(deferredValue) =>
        val promise = Promise[(ChildDeferredContext, Any, Vector[Throwable])]()

        def defer(d: Deferred[Any]) = {
          val (args, complexity) = calcComplexity(fieldsPath, astFields.head, field, userContext)
          Defer(promise, d, complexity, field, astFields, args)
        }

        val actualDeferred = deferredValue
          .map(d => Vector(defer(d)))
          .recover { case NonFatal(e) =>
            promise.failure(e)
            Vector.empty
          }

        SeqRes(
          promise.future.map { case (dctx, v, es) => SeqFutRes(v, es, dctx) }.recover { case e =>
            SeqFutRes(errors = Vector(e))
          },
          actualDeferred)
      case SequenceLeafAction(_) | _: MappedSequenceLeafAction[_, _, _] =>
        SeqRes(SeqFutRes(errors = Vector(new IllegalStateException(
          "Nested `SequenceLeafAction` is not yet supported inside of another `SequenceLeafAction`"))))
      case SubscriptionValue(_, _) =>
        SeqRes(
          SeqFutRes(errors = Vector(new IllegalStateException(
            "Subscription values are not supported for normal operations"))))
      case e =>
        SeqRes(
          SeqFutRes(errors = Vector(
            new IllegalStateException(s"${e.getClass.toString} is not supposed to appear here"))))
    }

  private def resolveActionsPar(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      actions: Actions,
      userCtx: Ctx,
      fieldsNamesOrdered: Vector[String]): Resolve = {
    val (errors, res) = actions

    res match {
      case None => Result(errors, None)
      case Some(results) =>
        val resolvedValues: Vector[(ast.Field, Resolve)] = results.map { case (astFields, rest) =>
          rest match {
            case None => astFields.head -> Result(ErrorRegistry.empty, None)
            case Some((field, updateCtx, v)) =>
              resolveLeafAction(path, tpe, userCtx, astFields, field, updateCtx)(v)
          }
        }

        val resSoFar =
          resolvedValues.iterator
            .collect { case (af, r: Result) => af -> r }
            .foldLeft(Result(errors, Some(marshaller.emptyMapNode(fieldsNamesOrdered)))) {
              case (res, (astField, other)) =>
                res.addToMap(
                  other,
                  astField.outputName,
                  isOptional(tpe, astField.name),
                  path.add(astField, tpe),
                  astField.location,
                  res.errors)
            }

        val complexRes = resolvedValues.collect { case (af, r: DeferredResult) => af -> r }

        if (complexRes.isEmpty) resSoFar.buildValue
        else {
          val allDeferred = complexRes.flatMap(_._2.deferred)
          val finalValue = Future
            .sequence(complexRes.iterator.map { case (astField, DeferredResult(_, future)) =>
              future.map(astField -> _)
            })
            .map { results =>
              results
                .foldLeft(resSoFar) { case (res, (astField, other)) =>
                  res.addToMap(
                    other,
                    astField.outputName,
                    isOptional(tpe, astField.name),
                    path.add(astField, tpe),
                    astField.location,
                    res.errors)
                }
                .buildValue
            }

          DeferredResult(allDeferred, finalValue)
        }
    }
  }

  private def resolveUc(newUc: Option[MappedCtxUpdate[Ctx, Any, Any]], v: Any, userCtx: Ctx) =
    newUc.fold(userCtx)(_.ctxFn(v))

  private def resolveVal(newUc: Option[MappedCtxUpdate[Ctx, Any, Any]], v: Any) = newUc match {
    case Some(MappedCtxUpdate(_, mapFn, _)) => mapFn(v)
    case None => v
  }

  private def resolveError(newUc: Option[MappedCtxUpdate[Ctx, Any, Any]], e: Throwable) = {
    try newUc.foreach(_.onError(e))
    catch {
      case NonFatal(ee) => ee.printStackTrace()
    }
    e
  }

  protected def resolveLeafAction(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      action: LeafAction[Ctx, Any]): (ast.Field, Resolve) =
    action match {
      case a: StandardLeafAction[Ctx, Any] =>
        resolveStandardLeafAction(path, tpe, userCtx, astFields, field, updateCtx)(a)
      case other => unresolvableLeafAction(path, tpe, astFields, updateCtx)(other)
    }

  protected def unresolvableLeafAction(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      astFields: Vector[ast.Field],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      action: LeafAction[Ctx, Any]): (ast.Field, Resolve) =
    illegalActionException(path, tpe, astFields, updateCtx)(
      s"Action of type '${action.getClass.toString}' is not supported by this resolver")

  protected def resolveStandardLeafAction(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      action: StandardLeafAction[Ctx, Any]): (ast.Field, Resolve) =
    action match {
      case v: Value[Ctx, Any] => resolveValue(path, tpe, userCtx, astFields, field, updateCtx)(v)
      case t: TryValue[Ctx, Any] =>
        resolveTryValue(path, tpe, userCtx, astFields, field, updateCtx)(t)
      case p: PartialValue[Ctx, Any] =>
        resolvePartialValue(path, tpe, userCtx, astFields, field, updateCtx)(p)
      case f: FutureValue[Ctx, Any] =>
        resolveFutureValue(path, tpe, userCtx, astFields, field, updateCtx)(f)
      case p: PartialFutureValue[Ctx, Any] =>
        resolvePartialFutureValue(path, tpe, userCtx, astFields, field, updateCtx)(p)
      case d: DeferredValue[Ctx, Any] =>
        resolveDeferredValue(path, tpe, userCtx, astFields, field, updateCtx)(d)
      case d: DeferredFutureValue[Ctx, Any] =>
        resolveDeferredFutureValue(path, tpe, userCtx, astFields, field, updateCtx)(d)
      case s: SequenceLeafAction[Ctx, Any] @unchecked =>
        resolveSequenceLeafAction(path, tpe, userCtx, astFields, field, updateCtx)(s)
      case _: MappedSequenceLeafAction[_, _, _] =>
        illegalActionException(path, tpe, astFields, updateCtx)(
          "MappedSequenceLeafAction is not supposed to appear here")
      case _: SubscriptionValue[_, _, _] =>
        illegalActionException(path, tpe, astFields, updateCtx)(
          "Subscription values are not supported for normal operations")
    }

  private def resolveValue(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      value: Value[Ctx, Any]): (ast.Field, Resolve) = {
    val v = value.value
    val fieldsPath = path.add(astFields.head, tpe)

    try
      astFields.head -> resolveValue(
        fieldsPath,
        astFields,
        field.fieldType,
        field,
        resolveVal(updateCtx, v),
        resolveUc(updateCtx, v, userCtx))
    catch {
      case NonFatal(e) =>
        astFields.head -> Result(
          ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
          None)
    }
  }

  private def resolveSequenceLeafAction(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      action: SequenceLeafAction[Ctx, Any]): (ast.Field, Resolve) = {
    val actions = action.value
    val fieldsPath = path.add(astFields.head, tpe)
    val values = resolveActionSequenceValues(fieldsPath, astFields, field, actions)
    val future = Future.sequence(values.map(_.value))

    val resolved = future
      .flatMap { vs =>
        val errors = vs.iterator.flatMap(_.errors).toVector
        val successfulValues = vs.collect { case SeqFutRes(v, _, _) if v != null => v }
        val dctx = vs.collect { case SeqFutRes(_, _, d) if d != null => d }

        def resolveDctx(resolve: Resolve) = {
          val last = dctx.lastOption
          val init = if (dctx.isEmpty) dctx else dctx.init

          resolve match {
            case res: Result =>
              dctx.foreach(_.promise.success(Vector.empty))
              Future.successful(res)
            case res: DeferredResult =>
              init.foreach(_.promise.success(Vector.empty))
              last.foreach(_.promise.success(res.deferred))
              res.futureValue
          }
        }

        errors.foreach(resolveError(updateCtx, _))

        if (successfulValues.size == vs.size)
          resolveDctx(
            resolveValue(
              fieldsPath,
              astFields,
              field.fieldType,
              field,
              resolveVal(updateCtx, successfulValues),
              resolveUc(updateCtx, successfulValues, userCtx))
              .appendErrors(fieldsPath, errors, astFields.head.location))
        else
          resolveDctx(
            Result(ErrorRegistry.empty.append(fieldsPath, errors, astFields.head.location), None))
      }
      .recover { case e =>
        Result(ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location), None)
      }

    val deferred = values.iterator.collect {
      case SeqRes(_, d, _) if d != null => d
    }.toVector
    val deferredFut = values.iterator.collect {
      case SeqRes(_, _, d) if d != null => d
    }.toVector

    astFields.head -> DeferredResult(Future.successful(deferred) +: deferredFut, resolved)
  }

  private def resolvePartialValue(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      p: PartialValue[Ctx, Any]): (ast.Field, Resolve) = {
    val v = p.value
    val es = p.errors

    val fieldsPath = path.add(astFields.head, tpe)

    es.foreach(resolveError(updateCtx, _))

    try
      astFields.head ->
        resolveValue(
          fieldsPath,
          astFields,
          field.fieldType,
          field,
          resolveVal(updateCtx, v),
          resolveUc(updateCtx, v, userCtx))
          .appendErrors(fieldsPath, es, astFields.head.location)
    catch {
      case NonFatal(e) =>
        astFields.head -> Result(
          ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location)
            .append(fieldsPath, es, astFields.head.location),
          None)
    }
  }

  private def resolveTryValue(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      t: TryValue[Ctx, Any]): (ast.Field, Resolve) = {
    val v = t.value
    val fieldsPath = path.add(astFields.head, tpe)

    v match {
      case Success(success) =>
        try
          astFields.head -> resolveValue(
            fieldsPath,
            astFields,
            field.fieldType,
            field,
            resolveVal(updateCtx, success),
            resolveUc(updateCtx, success, userCtx))
        catch {
          case NonFatal(e) =>
            astFields.head -> Result(
              ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
              None)
        }
      case Failure(e) =>
        astFields.head -> Result(
          ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
          None)
    }
  }

  private def resolveDeferredValue(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      d: DeferredValue[Ctx, Any]): (ast.Field, Resolve) = {
    val deferred = d.value
    val fieldsPath = path.add(astFields.head, tpe)
    val promise = Promise[(ChildDeferredContext, Any, Vector[Throwable])]()
    val (args, complexity) = calcComplexity(fieldsPath, astFields.head, field, userContext)
    val defer = Defer(promise, deferred, complexity, field, astFields, args)

    astFields.head -> DeferredResult(
      Vector(Future.successful(Vector(defer))),
      promise.future
        .flatMap { case (dctx, v, es) =>
          val uc = resolveUc(updateCtx, v, userCtx)

          es.foreach(resolveError(updateCtx, _))

          resolveValue(fieldsPath, astFields, field.fieldType, field, resolveVal(updateCtx, v), uc)
            .appendErrors(fieldsPath, es, astFields.head.location) match {
            case r: Result => dctx.resolveResult(r)
            case er: DeferredResult => dctx.resolveDeferredResult(er)
          }
        }
        .recover { case e =>
          Result(
            ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
            None)
        }
    )
  }

  protected def resolveFutureValue(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      f: FutureValue[Ctx, Any]): (ast.Field, Resolve) = {
    val future = f.value
    val fieldsPath = path.add(astFields.head, tpe)

    val resolved = future
      .map(v =>
        resolveValue(
          fieldsPath,
          astFields,
          field.fieldType,
          field,
          resolveVal(updateCtx, v),
          resolveUc(updateCtx, v, userCtx)))
      .recover { case e =>
        Result(ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location), None)
      }

    def process() = {
      val deferred = resolved.flatMap {
        case _: Result => Future.successful(Vector.empty)
        case r: DeferredResult => Future.sequence(r.deferred).map(_.flatten)
      }

      val value = resolved.flatMap {
        case r: Result => Future.successful(r)
        case dr: DeferredResult => dr.futureValue
      }

      astFields.head -> DeferredResult(Vector(deferred), value)
    }

    def processAndResolveDeferred() = {
      val value = resolved.flatMap {
        case r: Result => Future.successful(r)
        case dr: DeferredResult => immediatelyResolveDeferred(userContext, dr, identity)
      }

      astFields.head -> DeferredResult(Vector.empty, value)
    }

    deferredResolver.includeDeferredFromField match {
      case Some(fn) =>
        val (args, complexity) =
          calcComplexity(fieldsPath, astFields.head, field, userContext)

        if (fn(field, astFields, args, complexity))
          process()
        else
          processAndResolveDeferred()
      case None =>
        process()
    }
  }

  private def resolvePartialFutureValue(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      p: PartialFutureValue[Ctx, Any]): (ast.Field, Resolve) = {
    val future = p.value
    val fieldsPath = path.add(astFields.head, tpe)

    val resolved = future
      .map { case PartialValue(v, es) =>
        es.foreach(resolveError(updateCtx, _))

        resolveValue(
          fieldsPath,
          astFields,
          field.fieldType,
          field,
          resolveVal(updateCtx, v),
          resolveUc(updateCtx, v, userCtx))
          .appendErrors(fieldsPath, es, astFields.head.location)
      }
      .recover { case e =>
        Result(ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location), None)
      }

    val deferred = resolved.flatMap {
      case _: Result => Future.successful(Vector.empty)
      case r: DeferredResult => Future.sequence(r.deferred).map(_.flatten)
    }
    val value = resolved.flatMap {
      case r: Result => Future.successful(r)
      case dr: DeferredResult => dr.futureValue
    }

    astFields.head -> DeferredResult(Vector(deferred), value)
  }

  private def resolveDeferredFutureValue(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      d: DeferredFutureValue[Ctx, Any]): (ast.Field, Resolve) = {
    val deferredValue = d.value
    val fieldsPath = path.add(astFields.head, tpe)
    val promise = Promise[(ChildDeferredContext, Any, Vector[Throwable])]()

    def defer(d: Deferred[Any]) = {
      val (args, complexity) =
        calcComplexity(fieldsPath, astFields.head, field, userContext)
      Defer(promise, d, complexity, field, astFields, args)
    }

    val actualDeferred = deferredValue
      .map(d => Vector(defer(d)))
      .recover { case NonFatal(e) =>
        promise.failure(e)
        Vector.empty
      }

    astFields.head -> DeferredResult(
      Vector(actualDeferred),
      promise.future
        .flatMap { case (dctx, v, es) =>
          val uc = resolveUc(updateCtx, v, userCtx)

          es.foreach(resolveError(updateCtx, _))

          resolveValue(fieldsPath, astFields, field.fieldType, field, resolveVal(updateCtx, v), uc)
            .appendErrors(fieldsPath, es, astFields.head.location) match {
            case r: Result => dctx.resolveResult(r)
            case er: DeferredResult => dctx.resolveDeferredResult(er)
          }
        }
        .recover { case e =>
          Result(
            ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
            None)
        }
    )
  }

  private def illegalActionException(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      astFields: Vector[ast.Field],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(msg: String): (ast.Field, Resolve) = {
    val fieldsPath = path.add(astFields.head, tpe)
    val error = new IllegalStateException(msg)

    astFields.head -> Result(
      ErrorRegistry(fieldsPath, resolveError(updateCtx, error), astFields.head.location),
      None)
  }

  private def resolveDeferred(uc: Ctx, toResolve: Vector[Defer]): Unit =
    if (toResolve.nonEmpty) {
      @tailrec
      def findActualDeferred(deferred: Deferred[_]): Deferred[_] = deferred match {
        case MappingDeferred(d, _) => findActualDeferred(d)
        case d => d
      }

      def mapAllDeferred(
          deferred: Deferred[_],
          value: Future[Any]): Future[(Any, Vector[Throwable])] = deferred match {
        case MappingDeferred(d, fn) =>
          mapAllDeferred(d, value).map { case (v, errors) =>
            val (mappedV, newErrors) = fn.asInstanceOf[Any => (Any, Seq[Throwable])](v)
            mappedV -> (errors ++ newErrors)
          }
        case _ => value.map(_ -> Vector.empty)
      }

      try {
        val resolved = deferredResolver.resolve(
          toResolve.map(d => findActualDeferred(d.deferred)),
          uc,
          deferredResolverState)

        if (toResolve.size == resolved.size) {
          val dctx = ParentDeferredContext(uc, toResolve.size)

          for (i <- toResolve.indices) {
            val toRes = toResolve(i)

            toRes.promise.completeWith(
              mapAllDeferred(toRes.deferred, resolved(i))
                .map(v => (dctx.children(i), v._1, v._2))
                .recover { case NonFatal(e) =>
                  dctx.children(i).resolveError(e)
                  throw e
                })
          }

          dctx.init()
        } else {
          toResolve.foreach(_.promise.failure(new IllegalStateException(
            s"Deferred resolver returned ${resolved.size} elements, but it got ${toResolve.size} deferred values. This violates the contract. You can find more information in the documentation: https://sangria-graphql.github.io/learn/#deferred-values-and-resolver")))
        }
      } catch {
        case NonFatal(error) => toResolve.foreach(_.promise.failure(error))
      }
    }

  private def resolveValue(
      path: ExecutionPath,
      astFields: Vector[ast.Field],
      tpe: OutputType[_],
      field: Field[Ctx, _],
      value: Any,
      userCtx: Ctx,
      actualType: Option[InputType[_]] = None): Resolve =
    tpe match {
      case OptionType(optTpe) =>
        val actualValue = value match {
          case v: Option[_] => v
          case v => Option(v)
        }

        actualValue match {
          case Some(someValue) =>
            resolveValue(path, astFields, optTpe, field, someValue, userCtx, None)
          case None => Result(ErrorRegistry.empty, None)
        }
      case ListType(listTpe) =>
        if (isUndefinedValue(value))
          Result(ErrorRegistry.empty, None)
        else {
          val actualValue = value match {
            case seq: Iterable[_] => seq
            case other => Seq(other)
          }

          val res = actualValue.zipWithIndex.map { case (v, idx) =>
            resolveValue(path.withIndex(idx), astFields, listTpe, field, v, userCtx, None)
          }

          val simpleRes = res.collect { case r: Result => r }
          val optional = isOptional(listTpe)

          if (simpleRes.size == res.size)
            resolveSimpleListValue(simpleRes, path, optional, astFields.head.location)
          else {
            // this is very hot place, so resorting to mutability to minimize the footprint
            val deferredBuilder = new VectorBuilder[Future[Vector[Defer]]]
            val resultFutures = new VectorBuilder[Future[Result]]

            val resIt = res.iterator

            while (resIt.hasNext)
              resIt.next() match {
                case r: Result =>
                  resultFutures += Future.successful(r)
                case dr: DeferredResult =>
                  resultFutures += dr.futureValue
                  deferredBuilder ++= dr.deferred
              }

            DeferredResult(
              deferred = deferredBuilder.result(),
              futureValue = Future
                .sequence(resultFutures.result())
                .map(resolveSimpleListValue(_, path, optional, astFields.head.location))
            )
          }
        }
      case scalar: ScalarType[Any @unchecked] =>
        try
          Result(
            ErrorRegistry.empty,
            if (isUndefinedValue(value))
              None
            else {
              val coerced = scalar.coerceOutput(value, marshaller.capabilities)

              if (isUndefinedValue(coerced)) {
                None
              } else {
                val coercedWithMiddleware =
                  toScalarMiddleware match {
                    case Some(fn) => fn(coerced, actualType.getOrElse(scalar)).getOrElse(coerced)
                    case None => coerced
                  }

                Some(
                  marshalScalarValue(
                    coercedWithMiddleware,
                    marshaller,
                    scalar.name,
                    scalar.scalarInfo))
              }
            }
          )
        catch {
          case NonFatal(e) => Result(ErrorRegistry(path, e), None)
        }
      case scalar: ScalarAlias[Any @unchecked, Any @unchecked] =>
        resolveValue(
          path,
          astFields,
          scalar.aliasFor,
          field,
          scalar.toScalar(value),
          userCtx,
          Some(scalar))
      case enumT: EnumType[Any @unchecked] =>
        try
          Result(
            ErrorRegistry.empty,
            if (isUndefinedValue(value))
              None
            else {
              val coerced = enumT.coerceOutput(value)

              if (isUndefinedValue(coerced))
                None
              else
                Some(marshalEnumValue(coerced, marshaller, enumT.name))
            }
          )
        catch {
          case NonFatal(e) => Result(ErrorRegistry(path, e), None)
        }
      case obj: ObjectType[Ctx @unchecked, _] =>
        if (isUndefinedValue(value))
          Result(ErrorRegistry.empty, None)
        else
          fieldCollector.collectFields(path, obj, astFields) match {
            case Success(fields) =>
              val actions =
                collectActionsPar(path, obj, value, fields, ErrorRegistry.empty, userCtx)

              resolveActionsPar(path, obj, actions, userCtx, fields.namesOrdered)
            case Failure(error) => Result(ErrorRegistry(path, error), None)
          }
      case abst: AbstractType =>
        if (isUndefinedValue(value))
          Result(ErrorRegistry.empty, None)
        else {
          val actualValue =
            abst match {
              case abst: MappedAbstractType[Any @unchecked] => abst.contraMap(value)
              case _ => value
            }

          abst.typeOf(actualValue, schema) match {
            case Some(obj) =>
              resolveValue(path, astFields, obj, field, actualValue, userCtx, None)
            case None =>
              Result(
                ErrorRegistry(
                  path,
                  UndefinedConcreteTypeError(
                    path,
                    abst,
                    schema.possibleTypes.getOrElse(abst.name, Vector.empty),
                    actualValue,
                    exceptionHandler,
                    sourceMapper,
                    astFields.head.location.toList)
                ),
                None
              )
          }
        }
    }

  private def isUndefinedValue(value: Any) =
    value == null || value == None

  private def resolveSimpleListValue(
      simpleRes: Iterable[Result],
      path: ExecutionPath,
      optional: Boolean,
      astPosition: Option[AstLocation]): Result = {
    // this is very hot place, so resorting to mutability to minimize the footprint

    var errorReg = ErrorRegistry.empty
    val listBuilder = new VectorBuilder[marshaller.Node]
    var canceled = false
    val resIt = simpleRes.iterator

    while (resIt.hasNext && !canceled) {
      val res = resIt.next()

      if (!optional && res.value.isEmpty && res.errors.isEmpty)
        errorReg = errorReg.add(path, nullForNotNullTypeError(astPosition))
      else if (res.errors.nonEmpty)
        errorReg = errorReg.add(res.errors)

      res.nodeValue match {
        case node if optional =>
          listBuilder += marshaller.optionalArrayNodeValue(node)
        case Some(other) =>
          listBuilder += other
        case None =>
          canceled = true
      }
    }

    Result(errorReg, if (canceled) None else Some(marshaller.arrayNode(listBuilder.result())))
  }

  private def trackDeprecation(
      deprecationTracker: DeprecationTracker,
      ctx: Context[Ctx, _]): Unit = {
    val fieldArgs = ctx.args
    val visitedDirectives = mutable.Set[String]()

    def getArgValue(name: String, args: Args): Option[_] =
      if (args.argDefinedInQuery(name)) {
        if (args.optionalArgs.contains(name)) {
          args.argOpt(name)
        } else {
          Some(args.arg(name))
        }
      } else {
        None
      }

    def deprecatedArgsUsed(argDefs: List[Argument[_]], argValues: Args): List[Argument[_]] =
      argDefs.filter { argDef =>
        val argValue = getArgValue(argDef.name, argValues)
        argDef.deprecationReason.isDefined && argValue.isDefined
      }

    def trackDeprecatedDirectiveArgs(astDirective: ast.Directive): Unit = {
      // prevent infinite loop from directiveA -> arg -> directiveA -> arg ...
      if (visitedDirectives.contains(astDirective.name)) {
        return
      }
      visitedDirectives.add(astDirective.name)

      ctx.schema.directives.find(_.name == astDirective.name) match {
        case Some(directive) =>
          val directiveArgs = valueCollector
            .getArgumentValues(
              Some(astDirective),
              directive.arguments,
              astDirective.arguments,
              variables)

          directiveArgs match {
            case Success(directiveArgs) =>
              deprecatedArgsUsed(directive.arguments, directiveArgs).foreach { arg =>
                deprecationTracker.deprecatedDirectiveArgUsed(directive, arg, ctx)
              }
            case _ => // if we fail to get args, the query should fail elsewhere
          }

          // nested argument directives
          directive.arguments.foreach { nestedArg =>
            nestedArg.astDirectives.foreach(trackDeprecatedDirectiveArgs)
          }
        case _ => // do nothing
      }
    }

    def trackDeprecatedInputObjectFields(inputType: InputType[_], ioArg: Any): Unit =
      inputType match {
        case ioDef: InputObjectType[_] =>
          ioDef.fields.foreach { field =>
            // field deprecation
            val fieldVal: Option[_] = (ioArg match {
              case lm: ListMap[String, _] @unchecked => lm.get(field.name)
              case _ => None
            }) match {
              case Some(Some(nested)) => Some(nested)
              case value => value
            }

            if (field.deprecationReason.isDefined && fieldVal.isDefined) {
              deprecationTracker.deprecatedInputObjectFieldUsed(ioDef, field, ctx)
            }

            // for nested input objects
            if (fieldVal.isDefined) trackDeprecatedInputObjectFields(field.fieldType, fieldVal.get)

            // field directive args deprecation
            field.astDirectives.foreach(trackDeprecatedDirectiveArgs)
          }
        case OptionInputType(ofType) =>
          ioArg match {
            case Some(ioArg) => trackDeprecatedInputObjectFields(ofType, ioArg)
            case _ => trackDeprecatedInputObjectFields(ofType, ioArg)
          }
        case ListInputType(ofType) =>
          ioArg match {
            case seq: Seq[_] => seq.foreach(trackDeprecatedInputObjectFields(ofType, _))
            case _ => // do nothing
          }
        case _ => // do nothing
      }

    val field = ctx.field
    val astField = ctx.astFields.head

    // field deprecation
    val allFields =
      ctx.parentType.getField(ctx.schema, astField.name).asInstanceOf[Vector[Field[Ctx, Any]]]
    if (allFields.exists(_.deprecationReason.isDefined))
      deprecationTracker.deprecatedFieldUsed(ctx)

    // directive argument deprecation
    field.astDirectives.foreach(trackDeprecatedDirectiveArgs)

    // field argument deprecation
    deprecatedArgsUsed(field.arguments, fieldArgs).foreach { arg =>
      deprecationTracker.deprecatedFieldArgUsed(arg, ctx)
    }

    field.arguments.foreach { argDef =>
      // argument directives args deprecation
      argDef.astDirectives.foreach(trackDeprecatedDirectiveArgs)

      // input object field deprecation
      getArgValue(argDef.name, fieldArgs) match {
        case Some(ioArg) => trackDeprecatedInputObjectFields(argDef.argumentType, ioArg)
        case _ => // do nothing
      }
    }
  }

  private def resolveField(
      userCtx: Ctx,
      tpe: ObjectType[Ctx, _],
      path: ExecutionPath,
      value: Any,
      errors: ErrorRegistry,
      astFields: Vector[ast.Field]): FieldResolution = {
    val astField = astFields.head
    val allFields = tpe.getField(schema, astField.name).asInstanceOf[Vector[Field[Ctx, Any]]]
    val field = allFields.head

    maxQueryDepth match {
      case Some(max) if path.size > max =>
        ErrorFieldResolution(errors.add(path, MaxQueryDepthReachedError(max), astField.location))
      case _ =>
        valueCollector.getFieldArgumentValues(
          path,
          Some(astField),
          field.arguments,
          astField.arguments,
          variables) match {
          case Success(args) =>
            val ctx = Context[Ctx, Any](
              value = value,
              ctx = userCtx,
              args = args,
              schema = schema.asInstanceOf[Schema[Ctx, Any]],
              field = field,
              parentType = tpe.asInstanceOf[ObjectType[Ctx, Any]],
              marshaller = marshaller,
              query = queryAst,
              sourceMapper = sourceMapper,
              deprecationTracker = deprecationTracker,
              astFields = astFields,
              path = path,
              deferredResolverState = deferredResolverState
            )

            deprecationTracker.foreach(trackDeprecation(_, ctx))

            try {
              val mBefore = middleware.collect { case (mv, m: MiddlewareBeforeField[Ctx]) =>
                (m.beforeField(mv.asInstanceOf[m.QueryVal], middlewareCtx, ctx), mv, m)
              }

              val beforeAction = mBefore.collect {
                case (BeforeFieldResult(_, Some(action), _), _, _) => action
              }.lastOption
              val beforeAttachments = mBefore.iterator.collect {
                case (BeforeFieldResult(_, _, Some(att)), _, _) => att
              }.toVector
              val updatedCtx =
                if (beforeAttachments.nonEmpty) ctx.copy(middlewareAttachments = beforeAttachments)
                else ctx

              val mAfter = mBefore.filter(_._3.isInstanceOf[MiddlewareAfterField[Ctx]]).reverse
              val mError = mBefore.filter(_._3.isInstanceOf[MiddlewareErrorField[Ctx]])

              def doAfterMiddleware[Val](v: Val): Val =
                mAfter.foldLeft(v) {
                  case (acc, (BeforeFieldResult(cv, _, _), mv, m: MiddlewareAfterField[Ctx])) =>
                    m.afterField(
                      mv.asInstanceOf[m.QueryVal],
                      cv.asInstanceOf[m.FieldVal],
                      acc,
                      middlewareCtx,
                      updatedCtx)
                      .asInstanceOf[Option[Val]]
                      .getOrElse(acc)
                  case (acc, _) => acc
                }

              def doErrorMiddleware(error: Throwable): Unit =
                mError.foreach {
                  case (BeforeFieldResult(cv, _, _), mv, m: MiddlewareErrorField[Ctx]) =>
                    m.fieldError(
                      mv.asInstanceOf[m.QueryVal],
                      cv.asInstanceOf[m.FieldVal],
                      error,
                      middlewareCtx,
                      updatedCtx)
                  case _ => ()
                }

              def doAfterMiddlewareWithMap[Val, NewVal](fn: Val => NewVal)(v: Val): NewVal =
                mAfter.foldLeft(fn(v)) {
                  case (acc, (BeforeFieldResult(cv, _, _), mv, m: MiddlewareAfterField[Ctx])) =>
                    m.afterField(
                      mv.asInstanceOf[m.QueryVal],
                      cv.asInstanceOf[m.FieldVal],
                      acc,
                      middlewareCtx,
                      updatedCtx)
                      .asInstanceOf[Option[NewVal]]
                      .getOrElse(acc)
                  case (acc, _) => acc
                }

              try {
                def createResolution(result: Any): StandardFieldResolution =
                  result match {
                    // these specific cases are important for time measuring middleware and eager values
                    case resolved: Value[Ctx @unchecked, Any @unchecked] =>
                      StandardFieldResolution(
                        errors,
                        if (mAfter.nonEmpty)
                          Value(doAfterMiddleware(resolved.value))
                        else
                          resolved,
                        None)

                    case resolved: PartialValue[Ctx @unchecked, Any @unchecked] =>
                      StandardFieldResolution(
                        errors,
                        if (mAfter.nonEmpty)
                          PartialValue(doAfterMiddleware(resolved.value), resolved.errors)
                        else
                          resolved,
                        if (mError.nonEmpty)
                          Some(MappedCtxUpdate(_ => userCtx, identity, doErrorMiddleware))
                        else None
                      )

                    case resolved: TryValue[Ctx @unchecked, Any @unchecked] =>
                      StandardFieldResolution(
                        errors,
                        if (mAfter.nonEmpty && resolved.value.isSuccess)
                          Value(doAfterMiddleware(resolved.value.get))
                        else
                          resolved,
                        if (mError.nonEmpty)
                          Some(MappedCtxUpdate(_ => userCtx, identity, doErrorMiddleware))
                        else None
                      )

                    case res: SequenceLeafAction[Ctx @unchecked, _] =>
                      StandardFieldResolution(
                        errors,
                        res,
                        Some(
                          MappedCtxUpdate(
                            _ => userCtx,
                            if (mAfter.nonEmpty) doAfterMiddleware else identity,
                            if (mError.nonEmpty) doErrorMiddleware else identity)))

                    case res: MappedSequenceLeafAction[
                          Ctx @unchecked,
                          Any @unchecked,
                          Any @unchecked] =>
                      val mapFn = res.mapFn.asInstanceOf[Any => Any]

                      StandardFieldResolution(
                        errors,
                        res.action,
                        Some(
                          MappedCtxUpdate(
                            _ => userCtx,
                            if (mAfter.nonEmpty) doAfterMiddlewareWithMap(mapFn) else mapFn,
                            if (mError.nonEmpty) doErrorMiddleware else identity))
                      )

                    case resolved: LeafAction[Ctx @unchecked, Any @unchecked] =>
                      StandardFieldResolution(
                        errors,
                        resolved,
                        if (mAfter.nonEmpty || mError.nonEmpty)
                          Some(
                            MappedCtxUpdate(
                              _ => userCtx,
                              if (mAfter.nonEmpty) doAfterMiddleware else identity,
                              if (mError.nonEmpty) doErrorMiddleware else identity))
                        else None
                      )

                    case res: UpdateCtx[Ctx @unchecked, Any @unchecked] =>
                      StandardFieldResolution(
                        errors,
                        res.action,
                        Some(
                          MappedCtxUpdate(
                            res.nextCtx,
                            if (mAfter.nonEmpty) doAfterMiddleware else identity,
                            if (mError.nonEmpty) doErrorMiddleware else identity))
                      )

                    case res: MappedUpdateCtx[Ctx @unchecked, Any @unchecked, Any @unchecked] =>
                      StandardFieldResolution(
                        errors,
                        res.action,
                        Some(
                          MappedCtxUpdate(
                            res.nextCtx,
                            if (mAfter.nonEmpty) doAfterMiddlewareWithMap(res.mapFn) else res.mapFn,
                            if (mError.nonEmpty) doErrorMiddleware else identity))
                      )

                    case e => throw new IllegalStateException(s"Unsupported action: $e")
                  }

                val result =
                  beforeAction match {
                    case Some(action) => action
                    case None =>
                      field.resolve match {
                        case pfn: Projector[Ctx, Any, _] =>
                          pfn(updatedCtx, collectProjections(path, field, astFields, pfn.maxLevel))
                        case fn =>
                          fn(updatedCtx)
                      }
                  }
                result match {
                  case s: SubscriptionValue[Ctx @unchecked, _, _] =>
                    StreamFieldResolution(errors, s, createResolution)
                  case _ => createResolution(result)
                }
              } catch {
                case NonFatal(e) =>
                  try {
                    if (mError.nonEmpty) doErrorMiddleware(e)

                    ErrorFieldResolution(errors.add(path, e, astField.location))
                  } catch {
                    case NonFatal(me) =>
                      ErrorFieldResolution(
                        errors.add(path, e, astField.location).add(path, me, astField.location))
                  }
              }
            } catch {
              case NonFatal(e) => ErrorFieldResolution(errors.add(path, e, astField.location))
            }
          case Failure(error) => ErrorFieldResolution(errors.add(path, error))
        }
    }
  }

  private def collectProjections(
      path: ExecutionPath,
      field: Field[Ctx, _],
      astFields: Vector[ast.Field],
      maxLevel: Int): Vector[ProjectedName] = {

    def collectProjectionsInternal(
        path: ExecutionPath,
        tpe: OutputType[_],
        astFields: Vector[ast.Field],
        currLevel: Int): Vector[ProjectedName] =
      loop(path, tpe, astFields, currLevel)

    @tailrec
    def loop(
        path: ExecutionPath,
        tpe: OutputType[_],
        astFields: Vector[ast.Field],
        currLevel: Int): Vector[ProjectedName] =
      if (currLevel > maxLevel) Vector.empty
      else
        tpe match {
          case OptionType(ofType) => loop(path, ofType, astFields, currLevel)
          case ListType(ofType) => loop(path, ofType, astFields, currLevel)
          case objTpe: ObjectType[Ctx @unchecked, _] =>
            fieldCollector.collectFields(path, objTpe, astFields) match {
              case Success(ff) =>
                ff.fields.collect { case CollectedField(_, _, Success(astFields2)) =>
                  val astField = astFields2.head
                  val fields = objTpe.getField(schema, astField.name)
                  if (fields.isEmpty) Vector.empty
                  else {
                    val field = fields.head
                    if (field.tags.contains(ProjectionExclude)) Vector.empty
                    else {
                      val projectionNames = field.tags.iterator.collect {
                        case ProjectionName(name) => name
                      }

                      val projectedName =
                        if (projectionNames.nonEmpty) projectionNames
                        else Iterator.single(field.name)

                      projectedName.map { name =>
                        val children =
                          collectProjectionsInternal(
                            path.add(astField, objTpe),
                            field.fieldType,
                            astFields2,
                            currLevel + 1)
                        ProjectedName(name, children, Args(field, astField, variables))
                      }
                    }
                  }
                }.flatten
              case Failure(_) => Vector.empty
            }
          case abst: AbstractType =>
            schema.possibleTypes
              .get(abst.name)
              .map(
                _.flatMap(collectProjectionsInternal(path, _, astFields, currLevel + 1))
                  .groupBy(_.name)
                  .iterator
                  .map(_._2.head)
                  .toVector)
              .getOrElse(Vector.empty)
          case _ => Vector.empty
        }

    collectProjectionsInternal(path, field.fieldType, astFields, 1)
  }

  private def isOptional(tpe: ObjectType[_, _], fieldName: String): Boolean =
    isOptional(tpe.getField(schema, fieldName).head.fieldType)

  private def isOptional(tpe: OutputType[_]): Boolean =
    tpe.isInstanceOf[OptionType[_]]

  private def nullForNotNullTypeError(position: Option[AstLocation]) =
    new ExecutionError(
      "Cannot return null for non-nullable type",
      exceptionHandler,
      sourceMapper,
      position.toList)

  protected sealed trait Resolve {
    def appendErrors(
        path: ExecutionPath,
        errors: Vector[Throwable],
        position: Option[AstLocation]): Resolve
  }

  private case class DeferredResult(
      deferred: Vector[Future[Vector[Defer]]],
      futureValue: Future[Result])
      extends Resolve {
    def appendErrors(
        path: ExecutionPath,
        errors: Vector[Throwable],
        position: Option[AstLocation]): DeferredResult =
      if (errors.nonEmpty)
        copy(futureValue = futureValue.map(_.appendErrors(path, errors, position)))
      else this
  }

  private case class Defer(
      promise: Promise[(ChildDeferredContext, Any, Vector[Throwable])],
      deferred: Deferred[Any],
      complexity: Double,
      field: Field[_, _],
      astFields: Vector[ast.Field],
      args: Args)
      extends DeferredWithInfo
  private case class Result(
      errors: ErrorRegistry,
      value: Option[Any /* Either marshaller.Node or marshaller.MapBuilder */ ],
      userContext: Option[Ctx] = None)
      extends Resolve {
    def addToMap(
        other: Result,
        key: String,
        optional: Boolean,
        path: ExecutionPath,
        position: Option[AstLocation],
        updatedErrors: ErrorRegistry): Result =
      copy(
        errors =
          if (!optional && other.value.isEmpty && other.errors.isEmpty)
            updatedErrors.add(other.errors).add(path, nullForNotNullTypeError(position))
          else
            updatedErrors.add(other.errors),
        value =
          if (optional && other.value.isEmpty)
            value.map(v =>
              marshaller.addMapNodeElem(
                v.asInstanceOf[marshaller.MapBuilder],
                key,
                marshaller.nullNode,
                optional = false))
          else
            for {
              myVal <- value
              otherVal <- other.value
            } yield marshaller.addMapNodeElem(
              myVal.asInstanceOf[marshaller.MapBuilder],
              key,
              otherVal.asInstanceOf[marshaller.Node],
              optional = false)
      )

    def nodeValue: Option[marshaller.Node] = value.asInstanceOf[Option[marshaller.Node]]
    private def builderValue = value.asInstanceOf[Option[marshaller.MapBuilder]]
    def buildValue: Result = copy(value = builderValue.map(marshaller.mapNode))

    def appendErrors(
        path: ExecutionPath,
        e: Vector[Throwable],
        position: Option[AstLocation]): Result =
      if (e.nonEmpty) copy(errors = errors.append(path, e, position))
      else this
  }

  private case class ParentDeferredContext(uc: Ctx, expectedBranches: Int) {
    val children: Vector[ChildDeferredContext] =
      Vector.fill(expectedBranches)(ChildDeferredContext(Promise[Vector[Future[Vector[Defer]]]]()))

    def init(): Unit =
      Future.sequence(children.map(_.promise.future)).onComplete { res =>
        val allDeferred = res.get.flatten

        if (allDeferred.nonEmpty)
          resolveDeferredWithGrouping(allDeferred).foreach(groups =>
            groups.foreach(group => resolveDeferred(uc, group)))
      }
  }

  private case class ChildDeferredContext(promise: Promise[Vector[Future[Vector[Defer]]]]) {
    def resolveDeferredResult(res: DeferredResult): Future[Result] = {
      promise.success(res.deferred)
      res.futureValue
    }

    def resolveResult(res: Result): Future[Result] = {
      promise.success(Vector.empty)
      Future.successful(res)
    }

    def resolveError(e: Throwable): Unit =
      promise.success(Vector.empty)
  }

  private sealed trait FieldResolution
  private case class ErrorFieldResolution(errors: ErrorRegistry) extends FieldResolution
  private case class StandardFieldResolution(
      errors: ErrorRegistry,
      action: LeafAction[Ctx, Any],
      ctxUpdate: Option[MappedCtxUpdate[Ctx, Any, Any]])
      extends FieldResolution
  private case class StreamFieldResolution[Val, S[_]](
      errors: ErrorRegistry,
      value: SubscriptionValue[Ctx, Val, S],
      standardResolution: Any => StandardFieldResolution)
      extends FieldResolution

  private case class SeqRes(value: Future[SeqFutRes], defer: Defer, deferFut: Future[Vector[Defer]])

  private object SeqRes {
    def apply(value: SeqFutRes): SeqRes = SeqRes(Future.successful(value), null, null)
    def apply(value: SeqFutRes, defer: Defer): SeqRes =
      SeqRes(Future.successful(value), defer, null)
    def apply(value: SeqFutRes, deferFut: Future[Vector[Defer]]): SeqRes =
      SeqRes(Future.successful(value), null, deferFut)

    def apply(value: Future[SeqFutRes]): SeqRes = SeqRes(value, null, null)
    def apply(value: Future[SeqFutRes], defer: Defer): SeqRes = SeqRes(value, defer, null)
    def apply(value: Future[SeqFutRes], deferFut: Future[Vector[Defer]]): SeqRes =
      SeqRes(value, null, deferFut)
  }

  private case class SeqFutRes(
      value: Any = null,
      errors: Vector[Throwable] = Vector.empty,
      dctx: ChildDeferredContext = null)
}
