module StandardApi.Schema exposing
    ( Schema, Route, Model, Attribute
    , schemaDecoder, modelDecoder, attributeDecoder
    )

{-| Functions for working with the StandardAPI schema.

@docs Schema, Route, Model, Attribute
@docs schemaDecoder, modelDecoder, attributeDecoder

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (..)
import Json.Decode.Extra as Decode exposing (..)
import Json.Decode.Pipeline exposing (required)


{-| A StandardAPI schema definition.
-}
type alias Schema =
    { comment : String
    , models : Dict String Model
    , routes : List Route
    }


{-| A StandardAPI route definition.
-}
type alias Route =
    { path : String
    , method : String
    , model : Maybe Model
    , array : Bool
    , limit : Maybe Int
    }


{-| A StandardAPI model definition.
-}
type alias Model =
    { name : String
    , attributes : Dict String Attribute
    , comment : String
    }


{-| A StandardAPI attribute definition.
-}
type alias Attribute =
    { name : String
    , type_ : String
    , default : Maybe String
    , primaryKey : Bool
    , null : Bool
    , array : Bool
    , comment : String
    }


{-| Decode a JSON value into a `Schema`.
-}
schemaDecoder : Decoder Schema
schemaDecoder =
    Decode.map2
        (\comment models ->
            { comment = comment
            , models = models
            , routes = []
            }
        )
        (field "comment" (maybe string |> map (Maybe.withDefault "")))
        (field "models" (dict modelDecoder))
        |> Decode.andThen
            (\schema ->
                field "routes" (list (routeDecoder schema.models))
                    |> Decode.map (\routes -> { schema | routes = List.sortBy .path routes })
            )


routeDecoder : Dict String Model -> Decoder Route
routeDecoder models =
    Decode.succeed Route
        |> required "path" string
        |> required "method" string
        |> required "model" (maybe string |> map (Maybe.andThen (\name -> Dict.get name models)))
        |> required "array" bool
        |> required "limit" (maybe int)


{-| Decode a JSON value into a `Model`.
-}
modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed Model
        |> required "name" string
        |> required "attributes" (dict attributeDecoder)
        |> required "comment" (maybe string |> map (Maybe.withDefault ""))


{-| Decode a JSON value into a `Attribute`.
-}
attributeDecoder : Decoder Attribute
attributeDecoder =
    Decode.succeed Attribute
        |> required "name" string
        |> required "type" string
        |> required "default" (maybe string)
        |> required "primary_key" bool
        |> required "null" bool
        |> required "array" bool
        |> required "comment" (maybe string |> map (Maybe.withDefault ""))
