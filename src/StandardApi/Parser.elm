module StandardApi.Parser exposing
    ( bool, custom, float, int, iso8601, string
    , limit, offset, order
    )

{-| Decode query parameters of a StandardAPI query.

@docs bool, custom, float, int, iso8601, string
@docs limit, offset, order

-}

import Iso8601
import StandardApi exposing (Comparison(..), Direction(..), Limit, Offset, Order, Predicate)
import Time exposing (Posix)
import Url.Parser.Query as Query exposing (Parser)


{-| Decode a `Bool` query parameter of a StandardAPI query.
-}
bool : List String -> Parser (Predicate Bool)
bool =
    custom
        (\v ->
            case v of
                "true" ->
                    Just True

                "false" ->
                    Just False

                _ ->
                    Nothing
        )


{-| Decode a `Int` query parameter of a StandardAPI query.
-}
int : List String -> Parser (Predicate Int)
int =
    custom String.toInt


{-| Decode a `Float` query parameter of a StandardAPI query.
-}
float : List String -> Parser (Predicate Float)
float =
    custom String.toFloat


{-| Decode a `String` query parameter of a StandardAPI query.
-}
string : List String -> Parser (Predicate String)
string =
    custom (identity >> Just)


{-| Decode a `Posix` query parameter, represented as a ISO8610 date string, of a
StandardAPI query.
-}
iso8601 : List String -> Parser (Predicate Posix)
iso8601 =
    custom (Iso8601.toTime >> Result.toMaybe)


{-| Decode a [`Limit`](StandardApi#Limit) query parameter of a StandardAPI query.
-}
limit : Parser Limit
limit =
    Query.int "limit"


{-| Decode a [`Offset`](StandardApi#Offset) query parameter of a StandardAPI query.
-}
offset : Parser Offset
offset =
    Query.int "offset"


{-| Decode a [`Order`](StandardApi#Order) query parameter of a StandardAPI query.
-}
order : String -> Parser Order
order key =
    Query.custom ("order[" ++ key ++ "]") <|
        List.map
            (\order_ ->
                case order_ of
                    "asc" ->
                        ( key, Asc )

                    "desc" ->
                        ( key, Desc )

                    _ ->
                        ( key, Asc )
            )


{-| A custom decoder so you can decode other types of
a [`Predicate`](StandardApi#Predicate).

The [`iso8601`](#iso8601) for example is implemented like the follwing:

    iso8601 : List String -> Parser (Predicate Posix)
    iso8601 =
        custom (Iso8601.toTime >> Result.toMaybe)

-}
custom : (String -> Maybe a) -> List String -> Parser (Predicate a)
custom func path =
    let
        prefix =
            "where[" ++ String.join "][" path ++ "]"
    in
    List.foldl (Query.map2 (++))
        (Query.custom (prefix ++ "[]")
            (\v ->
                if v == [] then
                    []

                else
                    [ In (List.filterMap func v) ]
            )
        )
        [ Query.custom (prefix ++ "[lt]") (List.filterMap func >> List.map Lt)
        , Query.custom (prefix ++ "[lte]") (List.filterMap func >> List.map Lte)
        , Query.custom (prefix ++ "[eq]") (List.filterMap func >> List.map Eq)
        , Query.custom (prefix ++ "[gt]") (List.filterMap func >> List.map Gt)
        , Query.custom (prefix ++ "[gte]") (List.filterMap func >> List.map Gte)
        , Query.custom prefix
            (List.filterMap
                (\value ->
                    case value of
                        "true" ->
                            Just Set

                        "false" ->
                            Just Null

                        _ ->
                            Nothing
                )
            )
        ]
