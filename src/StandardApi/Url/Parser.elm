module StandardApi.Url.Parser exposing (..)

import Dict exposing (Dict)
import StandardApi exposing (Direction(..), Limit, Offset, Order, Query)
import Url
import Url.Parser


type alias Parser a b =
    Url.Parser.Parser a b

parse : Parser (a -> a) a -> Url -> Maybe a
parse parser url =


parseQuery : List String -> Maybe String -> Query
parserQuery ns queryString ->
    prepareQueryString queryString
        |> Dict.foldr
            (\k vs acc ->
                List.foldr
                    (\v subAcc ->
                        let
                            ( stack2, primaryKey ) =
                                String.foldr
                                    (\char ( subStack, currentKey ) ->
                                        case char of
                                            '[' ->
                                                ( currentKey :: subStack, "" )

                                            ']' ->
                                                if currentKey /= "" then
                                                    ( currentKey :: subStack, currentKey )

                                                else
                                                    ( subStack, currentKey )

                                            _ ->
                                                ( subStack, String.cons char currentKey )
                                     -- String.append currentKey (String.fromChar char)
                                    )
                                    ( [], "" )
                                    k

                            stack =
                                primaryKey :: stack2
                        in
                        case stack of
                            [ "limit" ] ->
                                { subAcc | limit = String.toInt v }

                            [ "offset" ] ->
                                { subAcc | offset = String.toInt v }

                            "order" :: subKeys ->
                                { subAcc
                                    | order =
                                        subAcc.order
                                            ++ [ ( String.join "." subKeys
                                                 , case v of
                                                    "asc" ->
                                                        Asc

                                                    "desc" ->
                                                        Desc

                                                    _ ->
                                                        Asc
                                                 )
                                               ]
                                }

                            "where" :: subKey ->
                                subAcc

                            _ ->
                                subAcc
                    )
                    acc
                    vs
            )
            { limit = Nothing
            , offset = Nothing
            , order = []
            , wheres = []
            }

limit : List String -> Parser Limit
limit ns =
    Maybe.map Just (QueryParser.int <| nsFun ns "limit")


nsFn : List String -> String -> String
nsFn ns v =
    case ns of
        [] ->
            v

        _ ->
            List.foldl (\ns_ acc -> acc ++ "[" ++ ns_ ++ "]") "" ns
                ++ "["
                ++ v
                ++ "]"

prepareQueryString : Maybe String -> Dict String (List String)
prepareQueryString maybeQuery =
    case maybeQuery of
        Nothing ->
            Dict.empty

        Just qry ->
            List.foldr addParam Dict.empty (String.split "&" qry)


addParam : String -> Dict String (List String) -> Dict String (List String)
addParam segment dict =
    case String.split "=" segment of
        [ rawKey, rawValue ] ->
            case Url.percentDecode rawKey of
                Nothing ->
                    dict

                Just key ->
                    case Url.percentDecode rawValue of
                        Nothing ->
                            dict

                        Just value ->
                            Dict.update key (addToParametersHelp value) dict

        _ ->
            dict


addToParametersHelp : a -> Maybe (List a) -> Maybe (List a)
addToParametersHelp value maybeList =
    case maybeList of
        Nothing ->
            Just [ value ]

        Just list ->
            Just (value :: list)
