# Elm StandardAPI Package

A package for interacting with a [StandardAPI](https://github.com/waratuman/standardapi)
backend.

# URL Builder

Build URLs with query parameters using `StandardApi.Url.Builder`:

```elm
import StandardApi exposing (..)
import StandardApi.Url.Builder as SaUrl
import Tree

SaUrl.absolute [ "accounts" ] emptyQuery
--> "/accounts"

SaUrl.absolute [ "accounts" ] { emptyQuery | limit = Just 20 }
--> "/accounts?limit=20"

SaUrl.absolute [ "accounts" ]
    { emptyQuery
        | limit = Just 20
        , order = [ ( "created_at", Desc ) ]
    }
--> "/accounts?order%5Bcreated_at%5D=desc&limit=20"

SaUrl.absolute [ "accounts" ]
    { emptyQuery
        | predicate = Just (Gte [ "login_count" ] (Int 100))
    }
--> "/accounts?where%5Blogin_count%5D%5Bgte%5D=100"

SaUrl.absolute [ "accounts" ]
    { emptyQuery
        | includes =
            [ Tree.tree (include ( "emails", emptyQuery )) [] ]
    }
--> "/accounts?include%5Bemails%5D=true"
```

# URL Parser

Parse URL query strings back into `Query` values using
`StandardApi.Url.Parser.Query`:

```elm
import StandardApi exposing (..)
import StandardApi.Type as Type
import StandardApi.Url.Parser.Query as Parser

-- Without type info, values are guessed from the raw string
Parser.parse [] "limit=10&order%5Bcreated_at%5D=desc"
--> Ok { emptyQuery | limit = Just 10, order = [ ( "created_at", Desc ) ] }

-- With type info, values are decoded according to the schema
Parser.parse [ ( "id", Type.Int ) ] "where%5Bid%5D%5Beq%5D=42"
--> Ok { emptyQuery | predicate = Just (Eq [ "id" ] (Int 42)) }

-- Type mismatches produce clear error messages
Parser.parse [ ( "id", Type.Int ) ] "where%5Bid%5D%5Beq%5D=abc"
--> Err "expected integer for id, got: abc"
```

If you have a `Model` from the schema, convert it to type mappings with
`modelToTypes`:

```elm
Parser.parse (modelToTypes model) queryString
```

You can also parse directly from a `Url`:

```elm
Parser.parseUrl [] url
```

# Example

```elm
import Json.Decode as Decode
import StandardApi exposing (..)
import StandardApi.Url.Builder as SaUrl
import Tree
import Url exposing (Protocol(..))

apiConfig : Config
apiConfig =
    { url =
        { protocol = Https
        , host = "example.com"
        , port_ = Nothing
        , path = ""
        , query = Nothing
        , fragment = Nothing
        }
    , headers =
        [ ( "Api-Key", "[TOKEN]" )
        ]
    , format = "application/json"
    , version = "0.1.0"
    , timeout = Nothing
    }

schemaRequest : (Result StandardApi.Error Schema -> msg) -> Cmd msg
schemaRequest msg =
    StandardApi.schemaRequest apiConfig
        { msg = msg
        , tracker = Nothing
        }

-- Using StandardApi.request with the query builder to construct the URL
accountsRequest :
    (Result StandardApi.Error (List ( Account, Email )) -> msg)
    -> Cmd msg
accountsRequest msg =
    let
        emailsQuery =
            { emptyQuery
                | predicate = Just (Set [ "primary" ])
            }

        accountQuery =
            { emptyQuery
                | limit = Just 20
                , order = [ ( "created_at", Desc ) ]
                , predicate = Just (Gte [ "login_count" ] (Int 100))
                , includes =
                    [ Tree.tree (include ( "emails", emailsQuery )) [] ]
            }
    in
    StandardApi.request apiConfig
        { method = "GET"
        , headers = []
        , path = SaUrl.absolute [ "accounts" ] accountQuery
        , body = StandardApi.emptyBody
        , expect =
            StandardApi.expectJson msg
                (Decode.list
                    (Account.decoder
                        |> Decode.andThen
                            (\account ->
                                Decode.field "emails" (Decode.index 0 Email.decoder)
                                    |> Decode.map (\email -> ( account, email ))
                            )
                    )
                )
        , tracker = Nothing
        }

-- Using Http.get directly with manual query parameters
accountsHttpRequest :
    (Result StandardApi.Error (List ( Account, Email )) -> msg)
    -> Cmd msg
accountsHttpRequest msg =
    Http.get
        { url =
            Url.Builder.absolute [ "accounts" ]
                [ Url.Builder.string "include[emails][where][primary]" "true"
                , Url.Builder.string "include[emails][limit]" "1"
                ]
        , expect =
            StandardApi.expectJson msg
                (Decode.list
                    (Account.decoder
                        |> Decode.andThen
                            (\account ->
                                Decode.field "emails" (Decode.index 0 Email.decoder)
                                    |> Decode.map (\email -> ( account, email ))
                            )
                    )
                )
        }
```
