module StandardApi.Url.Parser.Query exposing (parse, parseUrl)

{-| Parse URL query parameters back into a StandardAPI `Query`.

This is the inverse of `StandardApi.Url.Builder.query`.

The parser takes a list of `( String, Type )` pairs that map column names
to their types. For columns with a known type, the parser decodes values
accordingly (e.g., integers, booleans, timestamps). For unknown columns,
values are guessed from the raw string.

If a type is specified for a column and the raw string cannot be parsed as
that type, the parser returns an error.

@docs parse, parseUrl

-}

import Dict
import Iso8601
import StandardApi exposing (Direction(..), Include(..), Limit, Offset, Operation(..), Order, Query, Value(..), emptyQuery, include)
import StandardApi.Type as Type exposing (Type)
import Tree exposing (Tree, tree)
import Url exposing (Url)


{-| Parse a query string into a `Query` using the given type mappings.

    import StandardApi exposing (..)
    import StandardApi.Type as Type
    import StandardApi.Url.Parser.Query exposing (parse)

    parse [] ""
    --> Ok emptyQuery

    parse [] "limit=1"
    --> Ok { emptyQuery | limit = Just 1 }

    parse [ ( "id", Type.Int ) ] "where%5Bid%5D%5Beq%5D=42"
    --> Ok { emptyQuery | predicate = Just (Eq [ "id" ] (Int 42)) }

-}
parse : List ( String, Type ) -> String -> Result String Query
parse types queryString =
    let
        params =
            parseQueryString queryString
    in
    parseQuery types [] params


{-| Parse a `Url` into a `Query` using the given type mappings.

This is a convenience wrapper around `parse` that extracts the query
string from a `Url`.

-}
parseUrl : List ( String, Type ) -> Url -> Result String Query
parseUrl types url =
    parse types (Maybe.withDefault "" url.query)


parseQueryString : String -> List ( List String, String )
parseQueryString queryString =
    if String.isEmpty queryString then
        []

    else
        String.split "&" queryString
            |> List.filterMap
                (\pair ->
                    let
                        ( rawKey, rawValue ) =
                            splitOnFirstEquals pair
                    in
                    if String.isEmpty rawKey then
                        Nothing

                    else
                        let
                            key =
                                Maybe.withDefault rawKey (Url.percentDecode rawKey)

                            value =
                                Maybe.withDefault rawValue (Url.percentDecode rawValue)
                        in
                        Just ( parseBracketKey key, value )
                )


{-| Split a string on the first `=` character. This handles values that
contain `=` (e.g., base64-encoded values).
-}
splitOnFirstEquals : String -> ( String, String )
splitOnFirstEquals str =
    case String.indexes "=" str of
        [] ->
            ( str, "" )

        idx :: _ ->
            ( String.left idx str
            , String.dropLeft (idx + 1) str
            )


parseBracketKey : String -> List String
parseBracketKey key =
    case String.split "[" key of
        [] ->
            []

        first :: rest ->
            first
                :: List.map
                    (\segment ->
                        if String.endsWith "]" segment then
                            String.dropRight 1 segment

                        else
                            segment
                    )
                    rest


parseQuery : List ( String, Type ) -> List String -> List ( List String, String ) -> Result String Query
parseQuery types ns params =
    parseLimit ns params
        |> Result.andThen
            (\lim ->
                parseOffset ns params
                    |> Result.andThen
                        (\off ->
                            parseOrder ns params
                                |> Result.andThen
                                    (\ord ->
                                        parsePredicate types ns params
                                            |> Result.andThen
                                                (\pred ->
                                                    parseIncludes types ns params
                                                        |> Result.map
                                                            (\incl ->
                                                                { limit = lim
                                                                , offset = off
                                                                , order = ord
                                                                , predicate = pred
                                                                , includes = incl
                                                                }
                                                            )
                                                )
                                    )
                        )
            )


parseLimit : List String -> List ( List String, String ) -> Result String Limit
parseLimit ns params =
    case findValue (ns ++ [ "limit" ]) params of
        Nothing ->
            Ok Nothing

        Just str ->
            case String.toInt str of
                Just n ->
                    Ok (Just n)

                Nothing ->
                    Err ("expected integer for limit, got: " ++ str)


parseOffset : List String -> List ( List String, String ) -> Result String Offset
parseOffset ns params =
    case findValue (ns ++ [ "offset" ]) params of
        Nothing ->
            Ok Nothing

        Just str ->
            case String.toInt str of
                Just n ->
                    Ok (Just n)

                Nothing ->
                    Err ("expected integer for offset, got: " ++ str)


parseOrder : List String -> List ( List String, String ) -> Result String Order
parseOrder ns params =
    let
        prefix =
            ns ++ [ "order" ]

        results =
            params
                |> List.filterMap
                    (\( keys, value ) ->
                        case stripPrefix prefix keys of
                            Just (first :: rest) ->
                                Just ( String.join "." (first :: rest), value )

                            _ ->
                                Nothing
                    )
    in
    List.foldl
        (\( column, value ) acc ->
            acc
                |> Result.andThen
                    (\orders ->
                        case String.toLower value of
                            "asc" ->
                                Ok (orders ++ [ ( column, Asc ) ])

                            "desc" ->
                                Ok (orders ++ [ ( column, Desc ) ])

                            _ ->
                                Err ("expected asc or desc for order[" ++ column ++ "], got: " ++ value)
                    )
        )
        (Ok [])
        results


parsePredicate : List ( String, Type ) -> List String -> List ( List String, String ) -> Result String (Maybe Operation)
parsePredicate types ns params =
    let
        prefix =
            ns ++ [ "where" ]

        whereParams =
            params
                |> List.filterMap
                    (\( keys, value ) ->
                        stripPrefix prefix keys
                            |> Maybe.map (\rest -> ( rest, value ))
                    )
    in
    if List.isEmpty whereParams then
        Ok Nothing

    else
        parseWhereParams types whereParams
            |> Result.map Just


parseWhereParams : List ( String, Type ) -> List ( List String, String ) -> Result String Operation
parseWhereParams types params =
    let
        hasArrayPrefix =
            List.any
                (\( keys, _ ) ->
                    case keys of
                        "" :: _ ->
                            True

                        _ ->
                            False
                )
                params
    in
    if hasArrayPrefix then
        parseCompound types params

    else
        parseSinglePredicate types params


parseCompound : List ( String, Type ) -> List ( List String, String ) -> Result String Operation
parseCompound types params =
    let
        groups =
            splitByOr params
    in
    case groups of
        [] ->
            Err "empty compound predicate"

        [ single ] ->
            parseGroup types single
                |> Result.andThen combineWithConjunction

        first :: rest ->
            let
                foldGroup group accResult =
                    accResult
                        |> Result.andThen
                            (\acc ->
                                parseGroup types group
                                    |> Result.andThen combineWithConjunction
                                    |> Result.map (\g -> Disjunction acc g)
                            )
            in
            parseGroup types first
                |> Result.andThen combineWithConjunction
                |> (\firstResult -> List.foldl foldGroup firstResult rest)


splitByOr : List ( List String, String ) -> List (List ( List String, String ))
splitByOr params =
    let
        fold ( keys, value ) ( currentGroup, groups ) =
            if keys == [ "" ] && value == "OR" then
                ( [], List.reverse currentGroup :: groups )

            else
                ( ( keys, value ) :: currentGroup, groups )

        ( lastGroup, collectedGroups ) =
            List.foldl fold ( [], [] ) params
    in
    List.reverse (List.reverse lastGroup :: collectedGroups)


parseGroup : List ( String, Type ) -> List ( List String, String ) -> Result String (List Operation)
parseGroup types params =
    let
        stripped =
            List.map
                (\( keys, value ) ->
                    case keys of
                        "" :: rest ->
                            ( rest, value )

                        _ ->
                            ( keys, value )
                )
                params

        hasNestedArrays =
            List.any
                (\( keys, _ ) ->
                    case keys of
                        "" :: _ ->
                            True

                        _ ->
                            False
                )
                stripped
    in
    if hasNestedArrays then
        parseCompound types stripped
            |> Result.map List.singleton

    else
        groupIntoOperations types stripped


groupIntoOperations : List ( String, Type ) -> List ( List String, String ) -> Result String (List Operation)
groupIntoOperations types params =
    List.foldl
        (\( keys, value ) acc ->
            acc
                |> Result.andThen
                    (\ops ->
                        parseSingleOp types keys value
                            |> Result.map (\op -> ops ++ [ op ])
                    )
        )
        (Ok [])
        params


parseSinglePredicate : List ( String, Type ) -> List ( List String, String ) -> Result String Operation
parseSinglePredicate types params =
    case params of
        [ ( keys, value ) ] ->
            parseSingleOp types keys value

        _ ->
            groupIntoOperations types params
                |> Result.andThen combineWithConjunction


parseSingleOp : List ( String, Type ) -> List String -> String -> Result String Operation
parseSingleOp types keys value =
    case List.reverse keys of
        "eq" :: columnRev ->
            let
                col =
                    List.reverse columnRev
            in
            decodeValue types col value
                |> Result.map (Eq col)

        "not_eq" :: columnRev ->
            let
                col =
                    List.reverse columnRev
            in
            decodeValue types col value
                |> Result.map (Neq col)

        "lt" :: columnRev ->
            let
                col =
                    List.reverse columnRev
            in
            decodeValue types col value
                |> Result.map (Lt col)

        "lte" :: columnRev ->
            let
                col =
                    List.reverse columnRev
            in
            decodeValue types col value
                |> Result.map (Lte col)

        "gt" :: columnRev ->
            let
                col =
                    List.reverse columnRev
            in
            decodeValue types col value
                |> Result.map (Gt col)

        "gte" :: columnRev ->
            let
                col =
                    List.reverse columnRev
            in
            decodeValue types col value
                |> Result.map (Gte col)

        "ilike" :: columnRev ->
            let
                col =
                    List.reverse columnRev
            in
            decodeValue types col value
                |> Result.map (Ilike col)

        "contains" :: columnRev ->
            let
                col =
                    List.reverse columnRev
            in
            decodeValue types col value
                |> Result.map (Contains col)

        -- where[id][not_in][]=1 -> keys = ["id", "not_in", ""]
        "" :: "not_in" :: columnRev ->
            let
                col =
                    List.reverse columnRev
            in
            decodeValue types col value
                |> Result.map (\v -> NotIn col [ v ])

        -- where[tags][overlaps][]=elm -> keys = ["tags", "overlaps", ""]
        "" :: "overlaps" :: columnRev ->
            let
                col =
                    List.reverse columnRev
            in
            decodeValue types col value
                |> Result.map (\v -> Overlaps col [ v ])

        -- where[id][]=1 (In) -> keys = ["id", ""]
        "" :: columnRev ->
            let
                col =
                    List.reverse columnRev
            in
            decodeValue types col value
                |> Result.map (\v -> In col [ v ])

        _ ->
            -- Check for Dict values: where[col][op][dictKey]=value
            case splitOnOperator keys of
                Just ( col, op, dictKeys ) ->
                    let
                        dictValue =
                            decodeDictLeaf types col dictKeys value
                    in
                    dictValue
                        |> Result.map
                            (\v ->
                                case op of
                                    "eq" ->
                                        Eq col v

                                    "not_eq" ->
                                        Neq col v

                                    "lt" ->
                                        Lt col v

                                    "lte" ->
                                        Lte col v

                                    "gt" ->
                                        Gt col v

                                    "gte" ->
                                        Gte col v

                                    "ilike" ->
                                        Ilike col v

                                    "contains" ->
                                        Contains col v

                                    _ ->
                                        Eq col v
                            )

                Nothing ->
                    -- Null / Set: where[column]=false means Null, where[column]=true means Set
                    if value == "false" then
                        Ok (Null keys)

                    else if value == "true" then
                        Ok (Set keys)

                    else
                        decodeValue types keys value
                            |> Result.map (Eq keys)


{-| Try to find a known operator in the middle of a key path.
Returns (column, operator, remaining dict keys) if found.
For example, ["metadata", "eq", "key"] -> Just (["metadata"], "eq", ["key"])
-}
splitOnOperator : List String -> Maybe ( List String, String, List String )
splitOnOperator keys =
    let
        operators =
            [ "eq", "not_eq", "lt", "lte", "gt", "gte", "ilike", "contains" ]

        findOp before remaining =
            case remaining of
                [] ->
                    Nothing

                segment :: rest ->
                    if List.member segment operators && not (List.isEmpty before) && not (List.isEmpty rest) then
                        Just ( List.reverse before, segment, rest )

                    else
                        findOp (segment :: before) rest
    in
    findOp [] keys


{-| Decode a leaf value within a Dict structure, building the nested Dict.
For keys ["k1", "k2"] and value "v", produces Dict {"k1": Dict {"k2": String "v"}}
-}
decodeDictLeaf : List ( String, Type ) -> List String -> List String -> String -> Result String Value
decodeDictLeaf types col dictKeys rawValue =
    let
        leafValue =
            guessValue rawValue
    in
    Ok (buildDictValue dictKeys leafValue)


buildDictValue : List String -> Value -> Value
buildDictValue dictKeys leafValue =
    case dictKeys of
        [] ->
            leafValue

        key :: rest ->
            Dict (Dict.singleton key (buildDictValue rest leafValue))


{-| Decode a raw string value using the type mappings.
If the type is known and the value cannot be parsed, returns an error.
Falls back to heuristic guessing if the column is not in the type list.
-}
decodeValue : List ( String, Type ) -> List String -> String -> Result String Value
decodeValue types column str =
    case lookupType types column of
        Just Type.Int ->
            case String.toInt str of
                Just i ->
                    Ok (Int i)

                Nothing ->
                    Err ("expected integer for " ++ String.join "." column ++ ", got: " ++ str)

        Just Type.Float ->
            case String.toFloat str of
                Just f ->
                    Ok (Float f)

                Nothing ->
                    Err ("expected float for " ++ String.join "." column ++ ", got: " ++ str)

        Just Type.Bool ->
            case str of
                "true" ->
                    Ok (Bool True)

                "false" ->
                    Ok (Bool False)

                _ ->
                    Err ("expected boolean for " ++ String.join "." column ++ ", got: " ++ str)

        Just Type.Posix ->
            case Iso8601.toTime str of
                Ok posix ->
                    Ok (Posix posix)

                Err _ ->
                    Err ("expected ISO 8601 datetime for " ++ String.join "." column ++ ", got: " ++ str)

        Just Type.String ->
            Ok (String str)

        Just Type.Dict ->
            Ok (String str)

        Nothing ->
            Ok (guessValue str)


{-| Look up the type of a column by its path in the type list.
For simple columns like ["id"], matches on name.
For nested paths like ["metadata", "key"], uses the first segment.
-}
lookupType : List ( String, Type ) -> List String -> Maybe Type
lookupType types column =
    case column of
        first :: _ ->
            types
                |> List.filter (\( name, _ ) -> name == first)
                |> List.map (\( _, t ) -> t)
                |> List.head

        [] ->
            Nothing


{-| Guess a value type from a raw string when no schema info is available.
-}
guessValue : String -> Value
guessValue str =
    case String.toInt str of
        Just i ->
            Int i

        Nothing ->
            case String.toFloat str of
                Just f ->
                    if String.contains "." str then
                        Float f

                    else
                        String str

                Nothing ->
                    case str of
                        "true" ->
                            Bool True

                        "false" ->
                            Bool False

                        _ ->
                            String str


combineWithConjunction : List Operation -> Result String Operation
combineWithConjunction ops =
    case ops of
        [] ->
            Err "empty predicate group"

        [ single ] ->
            Ok single

        first :: rest ->
            let
                merged =
                    first
                        :: rest
                        |> mergeMultiValueOps
                        |> mergeDictOps
            in
            case merged of
                [] ->
                    Err "empty predicate group after merging"

                [ single_ ] ->
                    Ok single_

                first_ :: rest_ ->
                    Ok (List.foldl (\op acc -> Conjunction acc op) first_ rest_)


{-| Merge consecutive multi-value operations (In, NotIn, Overlaps) that share
the same column into a single operation with combined value lists.
-}
mergeMultiValueOps : List Operation -> List Operation
mergeMultiValueOps ops =
    let
        fold op ( merged, seen ) =
            case multiValueKey op of
                Just ( tag, col, vals ) ->
                    let
                        key =
                            ( tag, col )
                    in
                    if List.member key seen then
                        ( appendToMultiValue tag col vals merged, seen )

                    else
                        ( merged ++ [ op ], key :: seen )

                Nothing ->
                    ( merged ++ [ op ], seen )

        ( result, _ ) =
            List.foldl fold ( [], [] ) ops
    in
    result


multiValueKey : Operation -> Maybe ( String, List String, List Value )
multiValueKey op =
    case op of
        In col vals ->
            Just ( "In", col, vals )

        NotIn col vals ->
            Just ( "NotIn", col, vals )

        Overlaps col vals ->
            Just ( "Overlaps", col, vals )

        _ ->
            Nothing


appendToMultiValue : String -> List String -> List Value -> List Operation -> List Operation
appendToMultiValue tag col vals =
    List.map
        (\existing ->
            case ( tag, existing ) of
                ( "In", In ecol evals ) ->
                    if ecol == col then
                        In ecol (evals ++ vals)

                    else
                        existing

                ( "NotIn", NotIn ecol evals ) ->
                    if ecol == col then
                        NotIn ecol (evals ++ vals)

                    else
                        existing

                ( "Overlaps", Overlaps ecol evals ) ->
                    if ecol == col then
                        Overlaps ecol (evals ++ vals)

                    else
                        existing

                _ ->
                    existing
        )


{-| Merge Dict-valued operations on the same column into a single operation.
For example, Eq ["meta"] (Dict {"k1": v1}) and Eq ["meta"] (Dict {"k2": v2})
become Eq ["meta"] (Dict {"k1": v1, "k2": v2}).
-}
mergeDictOps : List Operation -> List Operation
mergeDictOps ops =
    let
        dictKey op =
            case op of
                Eq col (Dict _) ->
                    Just ( "Eq", col )

                Neq col (Dict _) ->
                    Just ( "Neq", col )

                Lt col (Dict _) ->
                    Just ( "Lt", col )

                Lte col (Dict _) ->
                    Just ( "Lte", col )

                Gt col (Dict _) ->
                    Just ( "Gt", col )

                Gte col (Dict _) ->
                    Just ( "Gte", col )

                _ ->
                    Nothing

        mergeDicts d1 d2 =
            Dict.foldl Dict.insert d1 d2

        extractDict op =
            case op of
                Eq _ (Dict d) ->
                    Just d

                Neq _ (Dict d) ->
                    Just d

                Lt _ (Dict d) ->
                    Just d

                Lte _ (Dict d) ->
                    Just d

                Gt _ (Dict d) ->
                    Just d

                Gte _ (Dict d) ->
                    Just d

                _ ->
                    Nothing

        updateWithDict tag col newDict =
            List.map
                (\existing ->
                    case ( dictKey existing, extractDict existing ) of
                        ( Just ( t, c ), Just d ) ->
                            if t == tag && c == col then
                                let
                                    merged =
                                        mergeDicts d newDict
                                in
                                case tag of
                                    "Eq" ->
                                        Eq col (Dict merged)

                                    "Neq" ->
                                        Neq col (Dict merged)

                                    "Lt" ->
                                        Lt col (Dict merged)

                                    "Lte" ->
                                        Lte col (Dict merged)

                                    "Gt" ->
                                        Gt col (Dict merged)

                                    "Gte" ->
                                        Gte col (Dict merged)

                                    _ ->
                                        existing

                            else
                                existing

                        _ ->
                            existing
                )

        fold op ( result, seen ) =
            case ( dictKey op, extractDict op ) of
                ( Just key, Just d ) ->
                    if List.member key seen then
                        ( updateWithDict (Tuple.first key) (Tuple.second key) d result, seen )

                    else
                        ( result ++ [ op ], key :: seen )

                _ ->
                    ( result ++ [ op ], seen )

        ( finalResult, _ ) =
            List.foldl fold ( [], [] ) ops
    in
    finalResult


parseIncludes : List ( String, Type ) -> List String -> List ( List String, String ) -> Result String (List (Tree Include))
parseIncludes types ns params =
    let
        prefix =
            ns ++ [ "include" ]

        includeParams =
            params
                |> List.filterMap
                    (\( keys, value ) ->
                        stripPrefix prefix keys
                            |> Maybe.map (\rest -> ( rest, value ))
                    )

        includeNames =
            includeParams
                |> List.filterMap
                    (\( keys, _ ) ->
                        List.head keys
                    )
                |> unique
    in
    List.foldl
        (\name acc ->
            acc
                |> Result.andThen
                    (\trees ->
                        let
                            scopedParams =
                                includeParams
                                    |> List.filterMap
                                        (\( keys, value ) ->
                                            case keys of
                                                first :: rest ->
                                                    if first == name then
                                                        Just ( rest, value )

                                                    else
                                                        Nothing

                                                [] ->
                                                    Nothing
                                        )

                            isSimple =
                                scopedParams == [ ( [], "true" ) ]
                        in
                        if isSimple then
                            Ok (trees ++ [ tree (include ( name, emptyQuery )) [] ])

                        else
                            parseSubIncludes types name scopedParams
                                |> Result.andThen
                                    (\subIncludeResult ->
                                        parseSubQuery types scopedParams subIncludeResult.consumedKeys
                                            |> Result.map
                                                (\subQ ->
                                                    trees ++ [ tree (include ( name, subQ )) subIncludeResult.trees ]
                                                )
                                    )
                    )
        )
        (Ok [])
        includeNames


type alias SubIncludeResult =
    { trees : List (Tree Include)
    , consumedKeys : List (List String)
    }


parseSubIncludes : List ( String, Type ) -> String -> List ( List String, String ) -> Result String SubIncludeResult
parseSubIncludes types _ scopedParams =
    let
        knownQueryPrefixes =
            [ "limit", "offset", "order", "where" ]

        subIncludeParams =
            scopedParams
                |> List.filter
                    (\( keys, _ ) ->
                        case keys of
                            first :: _ ->
                                not (List.member first knownQueryPrefixes)

                            [] ->
                                False
                    )

        subIncludeNames =
            subIncludeParams
                |> List.filterMap
                    (\( keys, _ ) ->
                        List.head keys
                    )
                |> unique

        consumed =
            List.map (\( keys, _ ) -> keys) subIncludeParams
    in
    List.foldl
        (\name acc ->
            acc
                |> Result.andThen
                    (\trees ->
                        let
                            innerParams =
                                subIncludeParams
                                    |> List.filterMap
                                        (\( keys, value ) ->
                                            case keys of
                                                first :: rest ->
                                                    if first == name then
                                                        Just ( rest, value )

                                                    else
                                                        Nothing

                                                [] ->
                                                    Nothing
                                        )

                            isSimple =
                                innerParams == [ ( [], "true" ) ]
                        in
                        if isSimple then
                            Ok (trees ++ [ tree (include ( name, emptyQuery )) [] ])

                        else
                            parseSubIncludes types name innerParams
                                |> Result.andThen
                                    (\subResult ->
                                        parseSubQuery types innerParams subResult.consumedKeys
                                            |> Result.map
                                                (\subQ ->
                                                    trees ++ [ tree (include ( name, subQ )) subResult.trees ]
                                                )
                                    )
                    )
        )
        (Ok [])
        subIncludeNames
        |> Result.map (\trees -> { trees = trees, consumedKeys = consumed })


parseSubQuery : List ( String, Type ) -> List ( List String, String ) -> List (List String) -> Result String Query
parseSubQuery types scopedParams consumedKeys =
    let
        queryParams =
            scopedParams
                |> List.filter
                    (\( keys, _ ) ->
                        not (List.member keys consumedKeys)
                    )
    in
    parseLimit [] queryParams
        |> Result.andThen
            (\lim ->
                parseOffset [] queryParams
                    |> Result.andThen
                        (\off ->
                            parseOrder [] queryParams
                                |> Result.andThen
                                    (\ord ->
                                        parsePredicate types [] queryParams
                                            |> Result.map
                                                (\pred ->
                                                    { limit = lim
                                                    , offset = off
                                                    , order = ord
                                                    , predicate = pred
                                                    , includes = []
                                                    }
                                                )
                                    )
                        )
            )



-- Helpers


findValue : List String -> List ( List String, String ) -> Maybe String
findValue target params =
    params
        |> List.filterMap
            (\( keys, value ) ->
                if keys == target then
                    Just value

                else
                    Nothing
            )
        |> List.head


stripPrefix : List String -> List String -> Maybe (List String)
stripPrefix prefix keys =
    case ( prefix, keys ) of
        ( [], rest ) ->
            Just rest

        ( p :: ps, k :: ks ) ->
            if p == k then
                stripPrefix ps ks

            else
                Nothing

        _ ->
            Nothing


unique : List String -> List String
unique list =
    List.foldl
        (\item acc ->
            if List.member item acc then
                acc

            else
                acc ++ [ item ]
        )
        []
        list
