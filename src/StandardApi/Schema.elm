module StandardApi.Schema exposing
    ( Schema, Route, Model, Column
    , schemaDecoder, modelDecoder, columnDecoder
    )

{-| Functions for working with the StandardAPI schema.

@docs Schema, Route, Model, Column
@docs schemaDecoder, modelDecoder, columnDecoder

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
    { columns : Dict String Column
    , comment : String
    }


{-| A StandardAPI column definition.
-}
type alias Column =
    { type_ : String
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
        |> required "columns" (dict columnDecoder)
        |> required "comment" (maybe string |> map (Maybe.withDefault ""))


{-| Decode a JSON value into a `Column`.
-}
columnDecoder : Decoder Column
columnDecoder =
    Decode.succeed Column
        |> required "type" string
        |> required "default" (maybe string)
        |> required "primary_key" bool
        |> required "null" bool
        |> required "array" bool
        |> required "comment" (maybe string |> map (Maybe.withDefault ""))
