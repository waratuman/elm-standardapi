module StandardApi.Url.Builder exposing (absolute, includes, limit, order, predicate, query, offset)

{-|


## Generation

@docs absolute, includes, limit, order, predicate, query, offset

-}

import Dict
import Iso8601
import StandardApi exposing (..)
import String
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
        , Maybe.withDefault [] (Maybe.map (predicate ns) query_.predicate)
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
        , Maybe.withDefault [] (Maybe.map (predicate ns) query_.predicate)
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


{-| Convert a `Operation` to a `QueryParameter` to send to a StandardAPI server.
-}
predicate : List String -> Operation -> List QueryParameter
predicate ns =
    predicateHelp (ns ++ [ "where" ])
        >> List.map
            (\( keys, value ) ->
                Builder.string (attrKey keys) value
            )


predicateHelp : List String -> Operation -> List ( List String, String )
predicateHelp ns op =
    case op of
        Conjunction a b ->
            predicateHelp (ns ++ [ "[]" ]) a
                ++ predicateHelp (ns ++ [ "[]" ]) b

        Disjunction a b ->
            predicateHelp (ns ++ [ "[]" ]) a
                ++ ( ns ++ [ "[]" ], "OR" )
                :: predicateHelp (ns ++ [ "[]" ]) b

        Ilike column value ->
            valueToParams (ns ++ column ++ [ "ilike" ]) value

        In column values ->
            List.map (valueToParams (ns ++ column ++ [ "[]" ])) values
                |> List.concat

        NotIn column values ->
            List.map (valueToParams (ns ++ column ++ [ "[not_in]" ])) values
                |> List.concat

        Lt column value ->
            valueToParams (ns ++ column ++ [ "lt" ]) value

        Lte column value ->
            valueToParams (ns ++ column ++ [ "lte" ]) value

        Eq column value ->
            valueToParams (ns ++ column ++ [ "eq" ]) value

        Gt column value ->
            valueToParams (ns ++ column ++ [ "gt" ]) value

        Gte column value ->
            valueToParams (ns ++ column ++ [ "gte" ]) value

        Null column ->
            [ ( ns ++ column, "false" ) ]

        Set column ->
            [ ( ns ++ column, "true" ) ]

        Overlaps column values ->
            List.map (valueToParams (ns ++ column ++ [ "[overlaps][]" ])) values
                |> List.concat

        Contains column value ->
            valueToParams (ns ++ column ++ [ "[contains]" ]) value


attrKey : List String -> String
attrKey ns =
    List.foldl
        (\attrName result ->
            if result == "" then
                attrName

            else if attrName == "[]" then
                result ++ attrName

            else
                result ++ "[" ++ attrName ++ "]"
        )
        ""
        ns


{-| Turn a `Value` into a set of query parameters.
-}
valueToParams : List String -> Value -> List ( List String, String )
valueToParams ns value =
    case value of
        Int v ->
            [ ( ns, String.fromInt v ) ]

        String v ->
            [ ( ns, v ) ]

        Float v ->
            [ ( ns, String.fromFloat v ) ]

        Bool True ->
            [ ( ns, "true" ) ]

        Bool False ->
            [ ( ns, "false" ) ]

        Posix v ->
            [ ( ns, Iso8601.fromTime v ) ]

        Dict v ->
            Dict.foldr
                (\k subValue acc ->
                    valueToParams (ns ++ [ k ]) subValue
                        ++ acc
                )
                []
                v
