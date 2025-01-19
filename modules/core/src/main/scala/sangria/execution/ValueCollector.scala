package sangria.execution

import sangria.ast
import sangria.marshalling._
import sangria.ast.SourceMapper
import sangria.renderer.QueryRenderer
import sangria.schema._
import sangria.util.Cache
import sangria.validation._

import scala.collection.immutable.VectorBuilder
import scala.util.{Failure, Success, Try}

class ValueCollector[Ctx, Input](
    schema: Schema[_, _],
    inputVars: Input,
    sourceMapper: Option[SourceMapper],
    deprecationTracker: Option[DeprecationTracker],
    userContext: Ctx,
    exceptionHandler: ExceptionHandler,
    fromScalarMiddleware: Option[(Any, InputType[_]) => Option[Either[Violation, Any]]],
    ignoreErrors: Boolean)(implicit um: InputUnmarshaller[Input]) {
  val coercionHelper =
    new ValueCoercionHelper[Ctx](sourceMapper, deprecationTracker, Some(userContext))

  private val argumentCache =
    Cache.empty[(ExecutionPath.PathCacheKeyReversed, Vector[ast.Argument]), Try[Args]]

  def getVariableValues(
      definitions: Vector[ast.VariableDefinition],
      fromScalarMiddleware: Option[(Any, InputType[_]) => Option[Either[Violation, Any]]]
  ): Try[Map[String, VariableValue]] = getVariableValues(definitions, fromScalarMiddleware, None)

  def getVariableValues(
      definitions: Vector[ast.VariableDefinition],
      fromScalarMiddleware: Option[(Any, InputType[_]) => Option[Either[Violation, Any]]],
      errorsLimit: Option[Int]
  ): Try[Map[String, VariableValue]] =
    if (!um.isMapNode(inputVars))
      Failure(
        new ExecutionError(
          s"Variables should be a map-like object, like JSON object. Got: ${um.render(inputVars)}",
          exceptionHandler))
    else {
      val res =
        definitions.foldLeft(
          (0, Vector.empty[(String, Either[Vector[Violation], VariableValue])])) {
          case (acc, varDef) =>
            val (accErrors, accResult) = acc

            // early termination if errors limit is defined and the current number of violations exceeds the limit
            if (errorsLimit.exists(_ <= accErrors)) acc
            else {
              val value = schema
                .getInputType(varDef.tpe)
                .map(coercionHelper.getVariableValue(
                  varDef,
                  _,
                  um.getRootMapValue(inputVars, varDef.name),
                  fromScalarMiddleware,
                  // calculate the allowed number of errors to be returned (if any)
                  errorsLimit.map(_ - accErrors)
                ))
                .getOrElse(
                  Left(
                    Vector(
                      UnknownVariableTypeViolation(
                        varDef.name,
                        QueryRenderer.render(varDef.tpe),
                        sourceMapper,
                        varDef.location.toList))))

              value match {
                case Right(Some(v)) => (accErrors, accResult :+ (varDef.name -> Right(v)))
                case Right(None) => acc
                case Left(violations) =>
                  // number of errors that is allowed to use (all if errors limit is not defined)
                  val errorsLeftToUse = errorsLimit.fold(violations.length) { limit =>
                    Math.min(violations.length, limit - accErrors)
                  }

                  (
                    accErrors + errorsLeftToUse,
                    accResult :+ (varDef.name -> Left(violations.take(errorsLeftToUse)))
                  )
              }
            }
        }

      val (errors, values) = res._2.partition(_._2.isLeft)

      if (errors.nonEmpty)
        Failure(
          VariableCoercionError(
            errors.collect { case (name, Left(errors)) => errors }.flatten,
            exceptionHandler))
      else Success(Map(values.collect { case (name, Right(v)) => name -> v }: _*))
    }

  def getFieldArgumentValues(
      path: ExecutionPath,
      forAstNode: Option[ast.AstNode],
      argumentDefs: List[Argument[_]],
      argumentAsts: Vector[ast.Argument],
      variables: Map[String, VariableValue]): Try[Args] =
    if (argumentDefs.isEmpty)
      ValueCollector.emptyArgs
    else
      argumentCache.getOrElseUpdate(
        path.cacheKeyReversed -> argumentAsts,
        getArgumentValues(forAstNode, argumentDefs, argumentAsts, variables))

  def getArgumentValues(
      forAstNode: Option[ast.AstNode],
      argumentDefs: List[Argument[_]],
      argumentAsts: Vector[ast.Argument],
      variables: Map[String, VariableValue]
  ): Try[Args] = ValueCollector.getArgumentValues(
    coercionHelper,
    forAstNode,
    argumentDefs,
    argumentAsts,
    variables,
    exceptionHandler,
    ignoreErrors,
    sourceMapper,
    fromScalarMiddleware)
}

object ValueCollector {
  private[execution] val emptyArgs = Success(Args.empty)

  def getArgumentValues[Ctx](
      coercionHelper: ValueCoercionHelper[Ctx],
      forAstNode: Option[ast.AstNode],
      argumentDefs: List[Argument[_]],
      argumentAsts: Vector[ast.Argument],
      variables: Map[String, VariableValue],
      exceptionHandler: ExceptionHandler,
      ignoreErrors: Boolean = false,
      sourceMapper: Option[SourceMapper] = None,
      fromScalarMiddleware: Option[(Any, InputType[_]) => Option[Either[Violation, Any]]] = None
  ): Try[Args] = {
    import coercionHelper._

    if (argumentDefs.isEmpty) emptyArgs
    else {
      val astArgMap = argumentAsts.groupBy(_.name).map { case (k, v) => (k, v.head) }
      val marshaller = CoercedScalaResultMarshaller.default
      val errors = new VectorBuilder[Violation]
      val defaultInfo = Cache.empty[String, Any]
      val undefinedArgs = Some(new VectorBuilder[String])

      val res = argumentDefs.foldLeft(
        marshaller.emptyMapNode(argumentDefs.map(_.name)): marshaller.MapBuilder) {
        case (acc, argDef) =>
          val argPath = argDef.name :: Nil
          val astValue = astArgMap.get(argDef.name).map(_.value)
          val fromInput = argDef.fromInput

          implicit val um = sangria.marshalling.queryAst.queryAstInputUnmarshaller

          try
            resolveMapValue(
              argDef.argumentType,
              argPath,
              argDef.defaultValue,
              forAstNode,
              argDef.name,
              marshaller,
              fromInput.marshaller,
              errors = errors,
              valueMap =
                (v: Any) => fromInput.fromResult(v.asInstanceOf[fromInput.marshaller.Node]),
              defaultValueInfo = Some(defaultInfo),
              undefinedValues = undefinedArgs,
              isArgument = true,
              fromScalarMiddleware = fromScalarMiddleware
            )(
              acc,
              astValue.map(
                coerceInputValue(
                  argDef.argumentType,
                  argPath,
                  _,
                  forAstNode,
                  Some(variables),
                  marshaller,
                  fromInput.marshaller,
                  fromScalarMiddleware = fromScalarMiddleware,
                  isArgument = true))
            )
          catch {
            case InputParsingError(e) =>
              errors ++= e.map(
                InvalidInputValueViolation(
                  argDef.name,
                  _,
                  sourceMapper,
                  astValue.flatMap(_.location).toList))
              acc
          }
      }

      val errorRes = errors.result()

      if (errorRes.nonEmpty && !ignoreErrors)
        Failure(AttributeCoercionError(errorRes, exceptionHandler))
      else {
        val optionalArgs = argumentDefs.filter(_.argumentType.isOptional).map(_.name).toSet
        val argsWithDefault = argumentDefs.filter(_.defaultValue.isDefined).map(_.name).toSet

        Success(
          Args(
            marshaller.mapNode(res).asInstanceOf[Map[String, Any]],
            argsWithDefault,
            optionalArgs,
            undefinedArgs.get.result().toSet,
            defaultInfo))
      }
    }
  }
}

case class VariableValue(
    fn: (ResultMarshaller, ResultMarshaller, InputType[_]) => Either[
      Vector[Violation],
      Trinary[ResultMarshaller#Node]]) {
  private val cache =
    Cache.empty[(Int, Int), Either[Vector[Violation], Trinary[ResultMarshaller#Node]]]

  def resolve(
      marshaller: ResultMarshaller,
      firstKindMarshaller: ResultMarshaller,
      actualType: InputType[_]): Either[Vector[Violation], Trinary[firstKindMarshaller.Node]] =
    cache
      .getOrElseUpdate(
        System.identityHashCode(firstKindMarshaller) -> System.identityHashCode(
          actualType.namedType),
        fn(marshaller, firstKindMarshaller, actualType))
      .asInstanceOf[Either[Vector[Violation], Trinary[firstKindMarshaller.Node]]]
}
