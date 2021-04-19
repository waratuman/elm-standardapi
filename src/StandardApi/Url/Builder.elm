module StandardApi.Url.Builder exposing
    ( absolute, includes, predicate, query, offset
    , limit, order
    )

{-|


## Generation

@docs absolute, includes, limit order, predicate, query, offset

-}

import Iso8601
import StandardApi exposing (..)
import String
import Time exposing (Posix)
import Tree exposing (Tree, children)
import Url.Builder as Builder exposing (QueryParameter)


{-| Create an absolute URL.let

    import StandardApi exposing (..)
    import StandardApi.Url.Builder exposing (..)

    absolute [ "packages" ] emptyQuery
    --> "/packages"

    absolute [ "packages" ] { emptyQuery | limit = Just 1 }
    --> "/packages?limit=1"

-}
absolute : List String -> Query -> String
absolute path query_ =
    Builder.absolute path (query [] query_)


{-| Convert a Query to a list of query parameters.
-}
query : List String -> Query -> List QueryParameter
query ns query_ =
    List.concat
        [ order ns query_.order
        , limit ns query_.limit
        , offset ns query_.offset
        , predicate ns query_.wheres
        , includes ns query_.includes
        ]


{-| This is to only be used for the `includes` function. This is because when including
and include, it is not currently namespaced like a normal query. For example,
including both the user and the org:

    -- usig this method
    include [ user ] [ org ] =
        true

    -- if we were to call the `query` method
    include [ user ] [ include ] [ org ] =
        true

Perhaps StandardAPI should be updated to handle both cases?

-}
subQuery : List String -> Query -> List QueryParameter
subQuery ns query_ =
    List.concat
        [ order ns query_.order
        , limit ns query_.limit
        , offset ns query_.offset
        , predicate ns query_.wheres
        , subIncludes ns query_.includes
        ]


{-| Convert an Order to a list of query parameters.

    import StandardApi exposing (..)
    import StandardApi.Url.Builder exposing (..)
    import Url.Builder

    order [] [ ( "created_at", Desc ) ]
    --> [ Url.Builder.string "order[created_at]" "desc" ]

-}
order : List String -> StandardApi.Order -> List QueryParameter
order ns order_ =
    List.map
        (\( column, direction ) ->
            let
                key =
                    attrKey (ns ++ [ "order", column ])

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


{-| Convert a Limit to a list of query parameters.

    import StandardApi exposing (..)
    import StandardApi.Url.Builder exposing (..)
    import Url.Builder

    limit [] (Just 1)
    --> [ Url.Builder.int "limit" 1 ]

-}
limit : List String -> Limit -> List QueryParameter
limit ns limit_ =
    case limit_ of
        Nothing ->
            []

        Just l ->
            [ Builder.int (attrKey (ns ++ [ "limit" ])) l ]


{-| Convert an Offset to a list of query parameters.

    import StandardApi exposing (..)
    import StandardApi.Url.Builder exposing (..)
    import Url.Builder

    offset [] (Just 1)
    --> [ Url.Builder.int "offset" 1 ]

-}
offset : List String -> Offset -> List QueryParameter
offset ns offset_ =
    case offset_ of
        Nothing ->
            []

        Just o ->
            [ Builder.int (attrKey (ns ++ [ "offset" ])) o ]


{-| Convert a Tree Include to a list of query parameters.

    import StandardApi exposing (..)
    import StandardApi.Url.Builder exposing (..)
    import Url.Builder
    import Tree exposing (tree)

    includes [] [ tree (include ("users", emptyQuery)) []  ]
    --> [ Url.Builder.string "include[users]" "true" ]

    includes [] [ tree (include ("users", emptyQuery))
                    [ tree (include ("account", emptyQuery)) [] ]
                ]
    --> [ Url.Builder.string "include[users][account]" "true" ]

    includes [] [ tree (include ("users", emptyQuery))
                    [ tree (include ("comments", { emptyQuery | limit = Just 2 } )) [] ]
                ]
    --> [ Url.Builder.int "include[users][comments][limit]" 2 ]

-}
includes : List String -> List (Tree Include) -> List QueryParameter
includes ns =
    List.foldr
        (\include acc ->
            let
                ( label, query_ ) =
                    unwrapInclude (Tree.label include)

                newNs =
                    ns ++ [ "include", label ]

                queryParams =
                    subQuery newNs query_
            in
            case ( queryParams, Tree.children include ) of
                ( [], [] ) ->
                    Builder.string (attrKey newNs) "true"
                        :: acc

                ( q, children ) ->
                    q ++ subIncludes newNs children ++ acc
        )
        []


{-| This is to only be used for the include query. This is because when including
and include, it is not currently namespaced like a normal query. For example,
including both the user and the org:

    -- usig this method
    include [ user ] [ org ] =
        true

    -- if we were to call the `query` method
    include [ user ] [ include ] [ org ] =
        true

Perhaps StandardAPI should be updated to handle both cases?

-}
subIncludes : List String -> List (Tree Include) -> List QueryParameter
subIncludes ns =
    List.foldr
        (\include acc ->
            let
                ( label, query_ ) =
                    unwrapInclude (Tree.label include)

                newNs =
                    ns ++ [ label ]

                queryParams =
                    subQuery newNs query_
            in
            case ( queryParams, Tree.children include ) of
                ( [], [] ) ->
                    Builder.string (attrKey newNs) "true"
                        :: acc

                ( q, children ) ->
                    q ++ subIncludes newNs children ++ acc
        )
        []



-- Predicate


{-| Convert a `Predicate` to a `QueryParameter` to send to a StandardAPI server.
-}
predicate : List String -> List Predicate -> List QueryParameter
predicate ns =
    List.foldl
        (\( column, operator ) acc ->
            acc
                ++ (let
                        attrKey_ =
                            attrKey (ns ++ [ "where", column ])

                        valueToString =
                            \value ->
                                case value of
                                    Int v ->
                                        String.fromInt v

                                    String v ->
                                        v

                                    Float v ->
                                        String.fromFloat v

                                    Bool True ->
                                        "true"

                                    Bool False ->
                                        "false"

                                    Posix v ->
                                        Iso8601.fromTime v
                    in
                    case operator of
                        Ilike value ->
                            [ Builder.string (attrKey_ ++ "[ilike]") (valueToString value) ]

                        In values ->
                            List.map (valueToString >> Builder.string (attrKey_ ++ "[]")) values

                        NotIn values ->
                            List.map (valueToString >> Builder.string (attrKey_ ++ "[not_in][]")) values

                        Lt value ->
                            [ Builder.string (attrKey_ ++ "[lt]") (valueToString value) ]

                        Lte value ->
                            [ Builder.string (attrKey_ ++ "[lte]") (valueToString value) ]

                        Eq value ->
                            [ Builder.string (attrKey_ ++ "[eq]") (valueToString value) ]

                        Gt value ->
                            [ Builder.string (attrKey_ ++ "[gt]") (valueToString value) ]

                        Gte value ->
                            [ Builder.string (attrKey_ ++ "[gte]") (valueToString value) ]

                        Null ->
                            [ Builder.string attrKey_ "false" ]

                        Set ->
                            [ Builder.string attrKey_ "true" ]

                        Overlaps values ->
                            List.map (valueToString >> Builder.string (attrKey_ ++ "[overlaps][]")) values
                   )
        )
        []


attrKey : List String -> String
attrKey ns =
    List.foldl
        (\attrName result ->
            if result == "" then
                attrName

            else
                result ++ "[" ++ attrName ++ "]"
        )
        ""
        ns
