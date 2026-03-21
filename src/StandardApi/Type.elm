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


{-| Parse a type string from the server into a `Type`.

    fromString "integer"
    --> Ok Int

    fromString "string"
    --> Ok String

    fromString "datetime"
    --> Ok Posix

    fromString "decimal"
    --> Ok Float

    fromString "boolean"
    --> Ok Bool

    fromString "hash"
    --> Ok Dict

-}
fromString : String -> Result String Type
fromString str =
    case str of
        "integer" ->
            Ok Int

        "string" ->
            Ok String

        "decimal" ->
            Ok Float

        "boolean" ->
            Ok Bool

        "datetime" ->
            Ok Posix

        "hash" ->
            Ok Dict

        _ ->
            Err ("unknown type: " ++ str)
