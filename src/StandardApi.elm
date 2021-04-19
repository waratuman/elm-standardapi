module StandardApi exposing
    ( Config
    , schemaRequest
    , Error(..)
    , errorToString, request, requestTask, cancel
    , emptyBody, jsonBody
    , expectJson, expectWhatever, jsonResolver
    , Query, Operator(..), Direction(..), Limit, Offset, Order, Predicate, Value(..), Include(..)
    , emptyQuery, include, unwrapInclude
    -- , bool, float, int, iso8601, predicate, string
    )

{-| Module for interfacing with StandardAPI.

For example you can make queries like the following:

    --> "https://example.com/accounts?where[emails][eq]=e%40mail.com&limit=1"




# Configuration

@docs Config


# Schema

@docs schemaRequest


# Requests

@docs Error
@docs errorToString, request, requestTask, cancel


# Body

@docs emptyBody, jsonBody


# Expect

@docs expectJson, expectWhatever, jsonResolver


# Querying

@docs Query, Operator, Direction, Limit, Offset, Order, Predicate, Value, Include
@docs emptyQuery, include, unwrapInclude


# Url generation


# Url parsing

-}

import Http exposing (Body, Expect, Header, Resolver)
import Json.Decode exposing (Decoder)
import Json.Encode as Encode
import StandardApi.Schema exposing (Schema)
import Task exposing (Task)
import Time exposing (Posix)
import Tree exposing (Tree)
import Url exposing (Url)
import Url.Builder as Builder exposing (QueryParameter)


{-| Error responses.
-}
type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int String
    | BadBody String


{-| Convert a `Error` into a `String` that is nice for debugging.
-}
errorToString : Error -> String
errorToString err =
    case err of
        BadUrl url ->
            "bad url (\"" ++ url ++ "\")"

        Timeout ->
            "network timeout"

        NetworkError ->
            "network error"

        BadStatus code message ->
            String.fromInt code
                ++ (case code of
                        400 ->
                            " Bad Request"

                        401 ->
                            " Unauthorized"

                        402 ->
                            " Payment Required"

                        403 ->
                            " Forbidden"

                        404 ->
                            " Not Found"

                        405 ->
                            " Method Not Allowed"

                        406 ->
                            " Not Acceptable"

                        407 ->
                            " Proxy Authentication Required"

                        408 ->
                            " Request Timeout"

                        409 ->
                            " Conflict"

                        410 ->
                            " Gone"

                        _ ->
                            ""
                   )
                ++ message

        BadBody body ->
            "bad body:\n" ++ body


{-| StandardAPI configuration.
-}
type alias Config =
    { url : Url
    , headers : List ( String, String )
    , timeout : Maybe Float
    , format : String
    , version : String
    }


type alias Query =
    { limit : Limit
    , order : Order
    , offset : Offset
    , wheres : List Predicate
    , includes : List (Tree Include)
    }


type Value
    = Int Int
    | String String
    | Float Float
    | Bool Bool
    | Posix Posix


{-| The `Operator` type is used for making comparinsons in a predicate.
-}
type Operator
    = Ilike Value
    | In (List Value)
    | NotIn (List Value)
    | Lt Value
    | Lte Value
    | Eq Value
    | Gt Value
    | Gte Value
    | Null
    | Set
    | Overlaps (List Value)


{-| The `Direction` type is used to order a query.
-}
type Direction
    = Asc
    | Desc


{-| The `Order` type is used to order a query. It is represented as a list
containing a tuple with the name of the colum and the direction to order.
-}
type alias Order =
    List ( String, Direction )


{-| The `Limit` type for limiting the results of a query. If the value is
`Nothing`, the default value is used (which may be unlimited). If the value is
a `Just x`, then the limit will be x.
-}
type alias Limit =
    Maybe Int


{-| The `Offset` type is for offseting the results of a query. If the value is
`Nothing`, there will be no offset (the same as `Just 0`). If the value is
a `Just x`, then the offset will be x.
-}
type alias Offset =
    Maybe Int


{-| A `Predicate` is a list of conditions to be used when building a query. If
this were SQL, it would be part the `WHERE` or `HAVING` clauses.
-}
type alias Predicate =
    ( String, Operator )


type Include
    = Include ( String, Query )


{-| Helper method to generate an include.

    import StandardApi exposing (..)

    include ("users", { emptyQuery | limit = Just 1 })
    --> Include ("users", { emptyQuery | limit = Just 1 })

-}
include : ( String, Query ) -> Include
include =
    Include


{-| Helper method to unwrap and include into the name of the include and its
subinclude. This is necessary due to recursion in the types.

    import StandardApi exposing (..)

    unwrapInclude (include ("users", { emptyQuery | limit = Just 1 }))
    --> ("users", { emptyQuery | limit = Just 1 })

-}
unwrapInclude : Include -> ( String, Query )
unwrapInclude (Include include_) =
    include_


{-| An empty query.
-}
emptyQuery : Query
emptyQuery =
    { limit = Nothing
    , order = []
    , offset = Nothing
    , wheres = []
    , includes = []
    }


{-| Create a HTTP request to a StandardAPI enpoint.

    import Url exposing (Protocol(..))

    config =
        { url =
            { protocol = Https
            , host = "example.com"
            , port_ = Nothing
            , path = ""
            , query = Nothing
            , fragment = Nothing
            }
        , headers = [ ( "Api-Key", "token") ]
        , timeout = Just 20
        , format = "application/json"
        , version = "0.1.0"
        }

    request config
        { method = "GET"
        , headers = []
        , path = "/posts"
        , body = emptyBody
        , msg = ReceivedPosts
        , decoder = list postDecoder
        , tracker = Nothing
        }

-}
request :
    Config
    ->
        { method : String
        , headers : List Header
        , path : String
        , body : Body
        , expect : Expect msg
        , tracker : Maybe String
        }
    -> Cmd msg
request config { method, headers, path, body, expect, tracker } =
    let
        baseUrl =
            config.url

        url =
            { baseUrl | path = path }
    in
    Http.riskyRequest
        { method = method
        , headers =
            ( "Accept", config.format )
                :: ( "Api-Version", config.version )
                :: config.headers
                |> List.map (\( n, v ) -> Http.header n v)
                |> List.append headers
        , url = Url.toString url
        , body = body
        , timeout = config.timeout
        , expect = expect
        , tracker = tracker
        }


{-| Try to cancel an ongoing request based on a tracker.
-}
cancel : String -> Cmd msg
cancel =
    Http.cancel


{-| Just like [`request`](#request), but it creates a `Task`.
-}
requestTask :
    Config
    ->
        { method : String
        , headers : List Header
        , path : String
        , body : Body
        , resolver : Resolver Error a
        }
    -> Task Error a
requestTask config { method, headers, path, body, resolver } =
    let
        baseUrl =
            config.url

        url =
            { baseUrl | path = path }
    in
    Http.riskyTask
        { method = method
        , headers =
            ( "Accept", config.format )
                :: ( "Api-Version", config.version )
                :: config.headers
                |> List.map (\( n, v ) -> Http.header n v)
                |> List.append headers
        , url = Url.toString url
        , body = body
        , resolver = resolver
        , timeout = config.timeout
        }


{-| Request the `Schema` from a StandardAPI resource.
-}
schemaRequest :
    Config
    ->
        { msg : Result Error Schema -> msg
        , tracker : Maybe String
        }
    -> Cmd msg
schemaRequest config { msg, tracker } =
    request config
        { method = "GET"
        , headers = []
        , path = "/schema"
        , body = Http.emptyBody
        , expect = expectJson msg StandardApi.Schema.schemaDecoder
        , tracker = tracker
        }


responseDecoder : (String -> Result String a) -> Http.Response String -> Result Error a
responseDecoder decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err <|
                BadStatus metadata.statusCode body

        Http.GoodStatus_ metadata body ->
            case decoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (BadBody err)


{-| Just like [`Http.expectWhatever`](https://package.elm-lang.org/packages/elm/http/latest/Http#expectWhatever),
but for decoding a JSON value in StandardAPI.
-}
expectWhatever : (Result Error () -> msg) -> Expect msg
expectWhatever toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    Err <|
                        BadStatus metadata.statusCode ""

                Http.GoodStatus_ metadata body ->
                    Ok ()


{-| Just like [`Http.expectJson`](https://package.elm-lang.org/packages/elm/http/latest/Http#expectJson),
but for decoding a JSON value in StandardAPI.
-}
expectJson : (Result Error a -> msg) -> Decoder a -> Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        responseDecoder
            (Json.Decode.decodeString decoder
                >> Result.mapError Json.Decode.errorToString
            )


{-| Just like [`Http.stringResolver`](https://package.elm-lang.org/packages/elm/http/latest/Http#stringResolver),
but for decoding a JSON value.
-}
jsonResolver : Decoder a -> Resolver Error a
jsonResolver decoder =
    Http.stringResolver
        (\response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    Err <|
                        BadStatus metadata.statusCode body

                Http.GoodStatus_ metadata body ->
                    case Json.Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (BadBody (Json.Decode.errorToString err))
        )


{-| Put a JSON value in the body of the request. This is simply an alias for
`Http.jsonBody` so you don't need to import `Http`.
-}
jsonBody : Encode.Value -> Body
jsonBody =
    Http.jsonBody


{-| An empty body for a request. This is simply an alias for
`Http.emptyBody` so you don't need to import `Http`.
-}
emptyBody : Body
emptyBody =
    Http.emptyBody
