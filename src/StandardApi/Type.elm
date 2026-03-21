module StandardApi.Type exposing (Type(..), fromString)

{-| Types for StandardAPI attributes.

@docs Type, fromString

-}


{-| The type of a StandardAPI attribute. This mirrors the value types in
`StandardApi.Value` and is used by the schema decoder and URL query parser
to determine how to interpret raw string values.
-}
type Type
    = Int
    | String
    | Float
    | Bool
    | Posix
    | Dict
    | Decimal


{-| Parse a type string from the server into a `Type`. Unrecognized types
default to `String` so that the attribute decoder never fails on unknown
PostgreSQL or Rails types.

    fromString "integer"
    --> Ok Int

    fromString "string"
    --> Ok String

    fromString "datetime"
    --> Ok Posix

    fromString "decimal"
    --> Ok Decimal

    fromString "boolean"
    --> Ok Bool

    fromString "hash"
    --> Ok Dict

    fromString "text"
    --> Ok String

    fromString "uuid"
    --> Ok String

    fromString "jsonb"
    --> Ok Dict

    fromString "numeric"
    --> Ok Float

    fromString "date"
    --> Ok String

    fromString "inet"
    --> Ok String

-}
fromString : String -> Result String Type
fromString str =
    case str of
        "integer" ->
            Ok Int

        "bigint" ->
            Ok Int

        "smallint" ->
            Ok Int

        "string" ->
            Ok String

        "text" ->
            Ok String

        "uuid" ->
            Ok String

        "inet" ->
            Ok String

        "cidr" ->
            Ok String

        "macaddr" ->
            Ok String

        "citext" ->
            Ok String

        "binary" ->
            Ok String

        "date" ->
            Ok String

        "time" ->
            Ok String

        "decimal" ->
            Ok Decimal

        "numeric" ->
            Ok Decimal

        "money" ->
            Ok Decimal

        "float" ->
            Ok Float

        "boolean" ->
            Ok Bool

        "datetime" ->
            Ok Posix

        "hash" ->
            Ok Dict

        "hstore" ->
            Ok Dict

        "json" ->
            Ok Dict

        "jsonb" ->
            Ok Dict

        _ ->
            Ok String
