module StandardApi.Url.Builder exposing (..)

import StandardApi exposing (Direction(..), Limit, Offset, Order, Query, order)
import Url.Builder as Builder exposing (QueryParameter)


absolute : List String -> Query a -> String
absolute path query_ =
    Builder.absolute path (query [] query_)


query : List String -> Query a -> List QueryParameter
query ns query_ =
    case query_ of
        Nothing ->
            []

        Just q ->
            List.concat
                [ order ns q.order
                , limit ns q.limit
                , offset ns q.offset
                ]


order : List String -> Order -> List QueryParameter
order ns order_ =
    List.map
        (\( column, direction ) ->
            let
                keyNs =
                    List.foldl (\attrName result -> result ++ "[" ++ attrName ++ "]")
                        ""
                        ns

                key =
                    keyNs ++ "order[" ++ column ++ "]"

                value =
                    case direction of
                        Asc ->
                            "asc"

                        Desc ->
                            "desc"
            in
            Builder.string key value
        )
        order_


limit : List String -> Limit -> List QueryParameter
limit ns limit_ =
    case limit_ of
        Nothing ->
            []

        Just l ->
            [ Builder.int (attrKey ns ++ "limit") l ]


offset : List String -> Offset -> List QueryParameter
offset ns offset_ =
    case offset_ of
        Nothing ->
            []

        Just o ->
            [ Builder.int (attrKey ns ++ "offset") o ]


attrKey : List String -> String
attrKey ns =
    List.foldl (\attrName result -> result ++ "[" ++ attrName ++ "]")
        ""
        ns
