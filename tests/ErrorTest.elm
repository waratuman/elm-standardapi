module ErrorTest exposing (..)

import Expect
import StandardApi exposing (..)
import Test exposing (..)


errorToStringTest : Test
errorToStringTest =
    describe "errorToString"
        [ test "BadStatus with known code and message" <|
            \() ->
                errorToString (BadStatus 404 "record not found")
                    |> Expect.equal "404 Not Found: record not found"
        , test "BadStatus with known code and empty message" <|
            \() ->
                errorToString (BadStatus 404 "")
                    |> Expect.equal "404 Not Found"
        , test "BadStatus with unknown code and message" <|
            \() ->
                errorToString (BadStatus 500 "internal server error")
                    |> Expect.equal "500: internal server error"
        , test "BadStatus with unknown code and empty message" <|
            \() ->
                errorToString (BadStatus 500 "")
                    |> Expect.equal "500"
        ]
