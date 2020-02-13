module StandardApi exposing
    ( Config
    , schemaRequest
    , Error(..)
    , errorToString, request, requestTask
    , Comparison(..), Direction(..), Limit, Offset, Order, Predicate
    , bool, float, int, iso8601, limit, offset, order, predicate, string
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
@docs errorToString, request, requestTask


# Querying

@docs Comparison, Direction, Limit, Offset, Order, Predicate


## Generation

@docs bool, float, int, iso8601, limit, offset, order, predicate, string


# Url generation


# Url parsing

-}

import Http exposing (Body, Expect, Header, Resolver)
import Iso8601
import Json.Decode exposing (Decoder)
import StandardApi.Schema exposing (Schema)
import Task exposing (Task)
import Time exposing (Posix)
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


{-| The `Comparison` type is used for making comparinsons in a predicate.
-}
type Comparison a
    = Ilike a
    | In (List a)
    | NotIn (List a)
    | Lt a
    | Lte a
    | Eq a
    | Gt a
    | Gte a
    | Null
    | Set
    | Overlaps (List a)


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


{-| A `Predicate` is a list of comparisons to be used when building a query. If
this were SQL, it would be the part the `WHERE` portion.
-}
type alias Predicate a =
    List (Comparison a)


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
        , msg : Result Error a -> msg
        , decoder : Decoder a
        , tracker : Maybe String
        }
    -> Cmd msg
request config { method, headers, path, body, msg, decoder, tracker } =
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
        , expect = expectJson msg decoder
        , tracker = tracker
        }


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
        , msg = msg
        , decoder = StandardApi.Schema.schemaDecoder
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


expectJson : (Result Error a -> msg) -> Decoder a -> Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        responseDecoder
            (Json.Decode.decodeString decoder
                >> Result.mapError Json.Decode.errorToString
            )


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


{-| Set the `limit` of a query parameter to send to a StandardAPI server.
-}
limit : List String -> Limit -> List QueryParameter
limit ns limit_ =
    case limit_ of
        Just l ->
            let
                attrKey =
                    List.foldl (\attrName result -> result ++ "[" ++ attrName ++ "]")
                        ""
                        ns
            in
            [ Builder.int (attrKey ++ "limit") l ]

        Nothing ->
            []


{-| Set the `offset` of a query parameter to send to a StandardAPI server.
-}
offset : List String -> Offset -> List QueryParameter
offset ns offset_ =
    case offset_ of
        Just l ->
            let
                attrKey =
                    List.foldl (\attrName result -> result ++ "[" ++ attrName ++ "]")
                        ""
                        ns
            in
            [ Builder.int (attrKey ++ "offset") l ]

        Nothing ->
            []


{-| Set the `order` of a query parameter to send to a StandardAPI server.

    order [] [ ( "created_at", Desc ) ]

-}
order : List String -> Order -> List QueryParameter
order ns =
    List.map <|
        \( attribute, direction ) ->
            let
                keyNs =
                    List.foldl (\attrName result -> result ++ "[" ++ attrName ++ "]")
                        ""
                        ns

                key =
                    keyNs ++ "order[" ++ attribute ++ "]"

                value =
                    case direction of
                        Asc ->
                            "asc"

                        Desc ->
                            "desc"
            in
            Builder.string key value


{-| Set the `predicate` (the where clause) of a parameter to send to a
StandardAPI server.
-}
predicate : (a -> String) -> List String -> Predicate a -> List QueryParameter
predicate func attributeNames =
    List.foldl
        (\condition acc ->
            acc
                ++ (let
                        attrKey =
                            List.foldl (\attrName result -> result ++ "[" ++ attrName ++ "]")
                                "where"
                                attributeNames
                    in
                    case condition of
                        Ilike value ->
                            [ Builder.string (attrKey ++ "[ilike]") (func value) ]

                        In values ->
                            List.map (func >> Builder.string (attrKey ++ "[]")) values

                        NotIn values ->
                            List.map (func >> Builder.string (attrKey ++ "[not_in][]")) values

                        Lt value ->
                            [ Builder.string (attrKey ++ "[lt]") (func value) ]

                        Lte value ->
                            [ Builder.string (attrKey ++ "[lte]") (func value) ]

                        Eq value ->
                            [ Builder.string (attrKey ++ "[eq]") (func value) ]

                        Gt value ->
                            [ Builder.string (attrKey ++ "[gt]") (func value) ]

                        Gte value ->
                            [ Builder.string (attrKey ++ "[gte]") (func value) ]

                        Null ->
                            [ Builder.string attrKey "false" ]

                        Set ->
                            [ Builder.string attrKey "true" ]

                        Overlaps values ->
                            List.map (func >> Builder.string (attrKey ++ "[overlaps][]")) values
                   )
        )
        []


{-| Set a `predicate`, where the value is a `String`, of a query parameter to
send to a StandardAPI Server.
-}
string : List String -> Predicate String -> List QueryParameter
string =
    predicate identity


{-| Set a `predicate`, where the value is a `Int`, of a query parameter to send
to a StandardAPI Server.
-}
int : List String -> Predicate Int -> List QueryParameter
int =
    predicate String.fromInt


{-| Set a `predicate`, where the value is a `Float`, of a query parameter to
send to a StandardAPI Server.
-}
float : List String -> Predicate Float -> List QueryParameter
float =
    predicate String.fromFloat


{-| Set a `predicate`, where the value is a `Bool`, of a query parameter to send
to a StandardAPI Server.
-}
bool : List String -> Predicate Bool -> List QueryParameter
bool =
    predicate
        (\v ->
            if v then
                "true"

            else
                "false"
        )


{-| Set a `predicate`, where the value is a `Posix` that is represented as a
ISO8601 string, of a query parameter to send to a StandardAPI Server.
-}
iso8601 : List String -> Predicate Posix -> List QueryParameter
iso8601 =
    predicate Iso8601.fromTime
