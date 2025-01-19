package sangria

import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.tag.@@
import sangria.marshalling.FromInput.CoercedScalaResult

import scala.annotation.tailrec

package object introspection {
  object TypeKind extends Enumeration {
    val Scalar, Object, Interface, Union, Enum, InputObject, List, NonNull = Value

    def fromString(kind: String): TypeKind.Value = kind match {
      case "SCALAR" => Scalar
      case "OBJECT" => Object
      case "INTERFACE" => Interface
      case "UNION" => Union
      case "ENUM" => Enum
      case "INPUT_OBJECT" => InputObject
      case "LIST" => List
      case "NON_NULL" => NonNull
    }
  }

  val __TypeKind = EnumType(
    "__TypeKind",
    Some("An enum describing what kind of type a given `__Type` is."),
    List(
      EnumValue(
        "SCALAR",
        value = TypeKind.Scalar,
        description = Some("Indicates this type is a scalar.")),
      EnumValue(
        "OBJECT",
        value = TypeKind.Object,
        description = Some(
          "Indicates this type is an object. " +
            "`fields` and `interfaces` are valid fields.")),
      EnumValue(
        "INTERFACE",
        value = TypeKind.Interface,
        description = Some(
          "Indicates this type is an interface. " +
            "`fields` and `possibleTypes` are valid fields.")),
      EnumValue(
        "UNION",
        value = TypeKind.Union,
        description = Some(
          "Indicates this type is a union. " +
            "`possibleTypes` is a valid field.")),
      EnumValue(
        "ENUM",
        value = TypeKind.Enum,
        description = Some(
          "Indicates this type is an enum. " +
            "`enumValues` is a valid field.")),
      EnumValue(
        "INPUT_OBJECT",
        value = TypeKind.InputObject,
        description = Some(
          "Indicates this type is an input object. " +
            "`inputFields` is a valid field.")),
      EnumValue(
        "LIST",
        value = TypeKind.List,
        description = Some(
          "Indicates this type is a list. " +
            "`ofType` is a valid field.")),
      EnumValue(
        "NON_NULL",
        value = TypeKind.NonNull,
        description = Some(
          "Indicates this type is a non-null. " +
            "`ofType` is a valid field."))
    )
  )

  val __DirectiveLocation = EnumType(
    "__DirectiveLocation",
    Some(
      "A Directive can be adjacent to many parts of the GraphQL language, a " +
        "__DirectiveLocation describes one such possible adjacencies."),
    List(
      EnumValue(
        "QUERY",
        value = DirectiveLocation.Query,
        description = Some("Location adjacent to a query operation.")),
      EnumValue(
        "MUTATION",
        value = DirectiveLocation.Mutation,
        description = Some("Location adjacent to a mutation operation.")),
      EnumValue(
        "SUBSCRIPTION",
        value = DirectiveLocation.Subscription,
        description = Some("Location adjacent to a subscription operation.")),
      EnumValue(
        "FIELD",
        value = DirectiveLocation.Field,
        description = Some("Location adjacent to a field.")),
      EnumValue(
        "FRAGMENT_DEFINITION",
        value = DirectiveLocation.FragmentDefinition,
        description = Some("Location adjacent to a fragment definition.")),
      EnumValue(
        "FRAGMENT_SPREAD",
        value = DirectiveLocation.FragmentSpread,
        description = Some("Location adjacent to a fragment spread.")),
      EnumValue(
        "INLINE_FRAGMENT",
        value = DirectiveLocation.InlineFragment,
        description = Some("Location adjacent to an inline fragment.")),
      EnumValue(
        "VARIABLE_DEFINITION",
        value = DirectiveLocation.VariableDefinition,
        description = Some("Location adjacent to a variable definition.")),
      EnumValue(
        "SCHEMA",
        value = DirectiveLocation.Schema,
        description = Some("Location adjacent to a schema definition.")),
      EnumValue(
        "SCALAR",
        value = DirectiveLocation.Scalar,
        description = Some("Location adjacent to a scalar definition.")),
      EnumValue(
        "OBJECT",
        value = DirectiveLocation.Object,
        description = Some("Location adjacent to an object type definition.")),
      EnumValue(
        "FIELD_DEFINITION",
        value = DirectiveLocation.FieldDefinition,
        description = Some("Location adjacent to a field definition.")),
      EnumValue(
        "ARGUMENT_DEFINITION",
        value = DirectiveLocation.ArgumentDefinition,
        description = Some("Location adjacent to an argument definition.")),
      EnumValue(
        "INTERFACE",
        value = DirectiveLocation.Interface,
        description = Some("Location adjacent to an interface definition.")),
      EnumValue(
        "UNION",
        value = DirectiveLocation.Union,
        description = Some("Location adjacent to a union definition.")),
      EnumValue(
        "ENUM",
        value = DirectiveLocation.Enum,
        description = Some("Location adjacent to an enum definition.")),
      EnumValue(
        "ENUM_VALUE",
        value = DirectiveLocation.EnumValue,
        description = Some("Location adjacent to an enum value definition.")),
      EnumValue(
        "INPUT_OBJECT",
        value = DirectiveLocation.InputObject,
        description = Some("INPUT_OBJECT")),
      EnumValue(
        "INPUT_FIELD_DEFINITION",
        value = DirectiveLocation.InputFieldDefinition,
        description = Some("Location adjacent to an input object field definition."))
    )
  )

  val __Field = ObjectType(
    name = "__Field",
    description = "Object and Interface types are described by a list of Fields, each of " +
      "which has a name, potentially a list of arguments, and a return type.",
    fieldsFn = () =>
      List[Field[Unit, Field[_, _]]](
        Field("name", StringType, resolve = _.value.name),
        Field("description", OptionType(StringType), resolve = _.value.description),
        Field(
          "args",
          ListType(__InputValue),
          arguments = includeDeprecated :: Nil,
          resolve = ctx => {
            val incDep = ctx.arg(includeDeprecated)

            if (incDep) {
              ctx.value.arguments
            } else {
              ctx.value.arguments.filter(_.deprecationReason.isEmpty)
            }
          }
        ),
        Field("type", __Type, resolve = false -> _.value.fieldType),
        Field("isDeprecated", BooleanType, resolve = _.value.deprecationReason.isDefined),
        Field("deprecationReason", OptionType(StringType), resolve = _.value.deprecationReason)
      )
  )

  val includeDeprecated = Argument[Option[Boolean @@ CoercedScalaResult], Boolean](
    "includeDeprecated",
    OptionInputType(BooleanType),
    false)

  private def getKind(value: (Boolean, Type)) = {
    @tailrec
    def identifyKind(t: Type, optional: Boolean): TypeKind.Value = t match {
      case OptionType(ofType) => identifyKind(ofType, true)
      case OptionInputType(ofType) => identifyKind(ofType, true)
      case _ if !optional => TypeKind.NonNull
      case _: ScalarType[_] => TypeKind.Scalar
      case _: ScalarAlias[_, _] => TypeKind.Scalar
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

  @tailrec
  private def findNamed(tpe: Type): Option[Type with Named] = tpe match {
    case o: OptionType[_] => findNamed(o.ofType)
    case o: OptionInputType[_] => findNamed(o.ofType)
    case l: ListType[_] => findNamed(l.ofType)
    case l: ListInputType[_] => findNamed(l.ofType)
    case n: Type with Named => Some(n)
    case _ => None
  }

  @tailrec
  private def findListType(tpe: Type): Option[Type] = tpe match {
    case o: OptionType[_] => findListType(o.ofType)
    case o: OptionInputType[_] => findListType(o.ofType)
    case l: ListType[_] => Some(l.ofType)
    case l: ListInputType[_] => Some(l.ofType)
    case _ => None
  }

  val __Type: ObjectType[Unit, (Boolean, Type)] = ObjectType(
    name = "__Type",
    description = "The fundamental unit of any GraphQL Schema is the type. There are " +
      "many kinds of types in GraphQL as represented by the `__TypeKind` enum." +
      "\n\nDepending on the kind of a type, certain fields describe " +
      "information about that type. Scalar types provide no information " +
      "beyond a name and description, while Enum types provide their values. " +
      "Object and Interface types provide the fields they describe. Abstract " +
      "types, Union and Interface, provide the Object types possible " +
      "at runtime. List and NonNull types compose other types.",
    fieldsFn = () =>
      List[Field[Unit, (Boolean, Type)]](
        Field("kind", __TypeKind, resolve = ctx => getKind(ctx.value)),
        Field(
          "name",
          OptionType(StringType),
          resolve = ctx =>
            getKind(ctx.value) match {
              case TypeKind.NonNull | TypeKind.List => None
              case _ => findNamed(ctx.value._2).map(_.name)
            }
        ),
        Field(
          "description",
          OptionType(StringType),
          resolve = ctx =>
            getKind(ctx.value) match {
              case TypeKind.NonNull | TypeKind.List => None
              case _ => findNamed(ctx.value._2).flatMap(_.description)
            }
        ),
        Field(
          "fields",
          OptionType(ListType(__Field)),
          arguments = includeDeprecated :: Nil,
          resolve = ctx => {
            val incDep = ctx.arg(includeDeprecated)
            val (_, tpe) = ctx.value

            tpe match {
              case t: ObjectLikeType[_, _] if incDep =>
                Some(t.uniqueFields.asInstanceOf[Vector[Field[_, _]]])
              case t: ObjectLikeType[_, _] =>
                Some(
                  t.uniqueFields
                    .asInstanceOf[Vector[Field[_, _]]]
                    .filter(_.deprecationReason.isEmpty))
              case _ => None
            }
          }
        ),
        Field(
          "interfaces",
          OptionType(ListType(__Type)),
          resolve = _.value._2 match {
            case t: ObjectLikeType[_, _] =>
              Some(t.allInterfaces.asInstanceOf[Vector[Type]].map(true -> _))
            case _ => None
          }
        ),
        Field(
          "possibleTypes",
          OptionType(ListType(__Type)),
          resolve = ctx =>
            ctx.value._2 match {
              case t: AbstractType =>
                ctx.schema.possibleTypes.get(t.name).map { tpe =>
                  t match {
                    case _: UnionType[_] => tpe.map(true -> _)
                    case _ => tpe.sortBy(_.name).map(true -> _)
                  }
                }
              case _ => None
            }
        ),
        Field(
          "enumValues",
          OptionType(ListType(__EnumValue)),
          arguments = includeDeprecated :: Nil,
          resolve = ctx => {
            val incDep = ctx.arg(includeDeprecated)

            ctx.value._2 match {
              case enumT: EnumType[_] if incDep => Some(enumT.values)
              case enumT: EnumType[_] => Some(enumT.values.filter(_.deprecationReason.isEmpty))
              case _ => None
            }
          }
        ),
        Field(
          "inputFields",
          OptionType(ListType(__InputValue)),
          arguments = includeDeprecated :: Nil,
          resolve = ctx => {
            val incDep = ctx.arg(includeDeprecated)

            ctx.value._2 match {
              case io: InputObjectType[_] if incDep => Some(io.fields)
              case io: InputObjectType[_] => Some(io.fields.filter(_.deprecationReason.isEmpty))
              case _ => None
            }
          }
        ),
        Field(
          "ofType",
          OptionType(__Type),
          resolve = ctx =>
            getKind(ctx.value) match {
              case TypeKind.NonNull => Some(true -> ctx.value._2)
              case TypeKind.List => findListType(ctx.value._2).map(false -> _)
              case _ => None
            }
        )
      )
  )

  val __InputValue: ObjectType[Unit, InputValue[_]] = ObjectType(
    name = "__InputValue",
    description = "Arguments provided to Fields or Directives and the input fields of an " +
      "InputObject are represented as Input Values which describe their type " +
      "and optionally a default value.",
    fields = List[Field[Unit, InputValue[_]]](
      Field("name", StringType, resolve = _.value.name),
      Field("description", OptionType(StringType), resolve = _.value.description),
      Field("type", __Type, resolve = false -> _.value.inputValueType),
      Field(
        "defaultValue",
        OptionType(StringType),
        description =
          Some("A GraphQL-formatted string representing the default value for this input value."),
        resolve = ctx =>
          ctx.value.defaultValue.flatMap(ctx.renderInputValueCompact(_, ctx.value.inputValueType))
      ),
      Field("isDeprecated", OptionType(BooleanType), resolve = _.value.deprecationReason.isDefined),
      Field("deprecationReason", OptionType(StringType), resolve = _.value.deprecationReason)
    )
  )

  val __EnumValue: ObjectType[Unit, EnumValue[_]] = ObjectType(
    name = "__EnumValue",
    description = "One possible value for a given Enum. Enum values are unique values, not " +
      "a placeholder for a string or numeric value. However an Enum value is " +
      "returned in a JSON response as a string.",
    fields = List[Field[Unit, EnumValue[_]]](
      Field("name", StringType, resolve = _.value.name),
      Field("description", OptionType(StringType), resolve = _.value.description),
      Field("isDeprecated", BooleanType, resolve = _.value.deprecationReason.isDefined),
      Field("deprecationReason", OptionType(StringType), resolve = _.value.deprecationReason)
    )
  )

  val __Directive = ObjectType(
    name = "__Directive",
    description = "A Directive provides a way to describe alternate runtime execution and " +
      "type validation behavior in a GraphQL document." +
      "\n\nIn some cases, you need to provide options to alter GraphQL’s " +
      "execution behavior in ways field arguments will not suffice, such as " +
      "conditionally including or skipping a field. Directives provide this by " +
      "describing additional information to the executor.",
    fields = fields[Unit, Directive](
      Field("name", StringType, resolve = _.value.name),
      Field("description", OptionType(StringType), resolve = _.value.description),
      Field(
        "locations",
        ListType(__DirectiveLocation),
        resolve = _.value.locations.toVector.sorted),
      Field(
        "args",
        ListType(__InputValue),
        arguments = includeDeprecated :: Nil,
        resolve = ctx => {
          val incDep = ctx.arg(includeDeprecated)

          if (incDep) {
            ctx.value.arguments
          } else {
            ctx.value.arguments.filter(_.deprecationReason.isEmpty)
          }
        }
      ),
      Field(
        "isRepeatable",
        BooleanType,
        Some("Permits using the directive multiple times at the same location."),
        resolve = _.value.repeatable)
    )
  )

  val __Schema = ObjectType(
    name = "__Schema",
    description = "A GraphQL Schema defines the capabilities of a GraphQL " +
      "server. It exposes all available types and directives on " +
      "the server, as well as the entry points for query, mutation, and subscription operations.",
    fields = List[Field[Unit, Schema[Any, Any]]](
      Field("description", OptionType(StringType), resolve = _.value.description),
      Field(
        "types",
        ListType(__Type),
        Some("A list of all types supported by this server."),
        resolve = _.value.typeList.map(true -> _)),
      Field(
        "queryType",
        __Type,
        Some("The type that query operations will be rooted at."),
        resolve = true -> _.value.query),
      Field(
        "mutationType",
        OptionType(__Type),
        Some(
          "If this server supports mutation, the type that mutation operations will be rooted at."),
        resolve = _.value.mutation.map(true -> _)
      ),
      Field(
        "subscriptionType",
        OptionType(__Type),
        Some(
          "If this server support subscription, the type that subscription operations will be rooted at."),
        resolve = _.value.subscription.map(true -> _)
      ),
      Field(
        "directives",
        ListType(__Directive),
        Some("A list of all directives supported by this server."),
        resolve = _.value.directives)
    )
  )

  val SchemaMetaField: Field[Unit, Unit] = Field(
    name = "__schema",
    fieldType = __Schema,
    description = Some("Access the current type schema of this server."),
    resolve = _.schema.asInstanceOf[Schema[Any, Any]]
  )

  val TypeMetaField: Field[Unit, Unit] = Field(
    name = "__type",
    fieldType = OptionType(__Type),
    description = Some("Request the type information of a single type."),
    arguments = Argument("name", StringType) :: Nil,
    resolve = ctx => ctx.schema.types.get(ctx.arg[String]("name")).map(true -> _._2)
  )

  val TypeNameMetaField: Field[Unit, Unit] = Field(
    name = "__typename",
    fieldType = StringType,
    description = Some("The name of the current Object type at runtime."),
    resolve = ctx => ctx.parentType.name)

  val MetaFieldNames = Set(SchemaMetaField.name, TypeMetaField.name, TypeNameMetaField.name)

  def isIntrospection(tpe: CompositeType[_], field: Field[_, _]): Boolean =
    Schema.isIntrospectionType(tpe.name) || MetaFieldNames.contains(field.name)

  val IntrospectionTypes: List[Type with Named] =
    __Schema :: __TypeKind :: __DirectiveLocation :: __Type :: __Field :: __InputValue :: __EnumValue :: __Directive :: Nil

  val IntrospectionTypesByName: Map[String, Type with Named] =
    IntrospectionTypes.groupBy(_.name).map { case (k, v) => (k, v.head) }

  def introspectionQuery: ast.Document = introspectionQuery()

  def introspectionQuery(
      schemaDescription: Boolean = true,
      directiveRepeatableFlag: Boolean = true,
      inputValueDeprecation: Boolean = false): ast.Document =
    QueryParser
      .parse(
        introspectionQueryString(schemaDescription, directiveRepeatableFlag, inputValueDeprecation))
      .get

  def introspectionQueryString(
      schemaDescription: Boolean = true,
      directiveRepeatableFlag: Boolean = true,
      inputValueDeprecation: Boolean = false): String =
    s"""query IntrospectionQuery {
       |  __schema {
       |    queryType { name }
       |    mutationType { name }
       |    subscriptionType { name }
       |    types {
       |      ...FullType
       |    }
       |    directives {
       |      name
       |      description
       |      locations
       |      args${if (inputValueDeprecation) "(includeDeprecated: true)" else ""} {
       |        ...InputValue
       |      }
       |      ${if (directiveRepeatableFlag) "isRepeatable" else ""}
       |    }
       |    ${if (schemaDescription) "description" else ""}
       |  }
       |}
       |fragment FullType on __Type {
       |  kind
       |  name
       |  description
       |  fields(includeDeprecated: true) {
       |    name
       |    description
       |    args${if (inputValueDeprecation) "(includeDeprecated: true)" else ""} {
       |      ...InputValue
       |    }
       |    type {
       |      ...TypeRef
       |    }
       |    isDeprecated
       |    deprecationReason
       |  }
       |  inputFields${if (inputValueDeprecation) "(includeDeprecated: true)" else ""} {
       |    ...InputValue
       |  }
       |  interfaces {
       |    ...TypeRef
       |  }
       |  enumValues(includeDeprecated: true) {
       |    name
       |    description
       |    isDeprecated
       |    deprecationReason
       |  }
       |  possibleTypes {
       |    ...TypeRef
       |  }
       |}
       |fragment InputValue on __InputValue {
       |  name
       |  description
       |  type { ...TypeRef }
       |  defaultValue
       ${if (inputValueDeprecation) """|  isDeprecated
       |  deprecationReason""" else "|"}
       |}
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
       |        ofType {
       |          kind
       |          name
       |          ofType {
       |            kind
       |            name
       |            ofType {
       |              kind
       |              name
       |              ofType {
       |                kind
       |                name
       |              }
       |            }
       |          }
       |        }
       |      }
       |    }
       |  }
       |}""".stripMargin
}
