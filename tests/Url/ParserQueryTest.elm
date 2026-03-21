module Url.ParserQueryTest exposing (..)

import Dict
import Expect
import Iso8601
import StandardApi exposing (..)
import StandardApi.Type as Type
import StandardApi.Url.Builder as Builder
import StandardApi.Url.Parser.Query as Parser
import Test exposing (..)
import Time
import Tree exposing (tree)
import Url


{-| No type mappings -- parser falls back to guessing types.
-}
noTypes : List ( String, Type.Type )
noTypes =
    []


{-| Type mappings for testing schema-driven decoding.
-}
typedColumns : List ( String, Type.Type )
typedColumns =
    [ ( "id", Type.Int )
    , ( "name", Type.String )
    , ( "age", Type.Int )
    , ( "price", Type.Float )
    , ( "active", Type.Bool )
    , ( "created_at", Type.Posix )
    , ( "tags", Type.String )
    , ( "region_ids", Type.Int )
    , ( "deleted_at", Type.Posix )
    , ( "status", Type.String )
    , ( "token", Type.String )
    , ( "version", Type.Int )
    , ( "login_count", Type.Int )
    ]


suite : Test
suite =
    describe "StandardApi.Url.Parser.Query"
        [ basicTests
        , orderTests
        , predicateTests
        , schemaDecodingTests
        , includeTests
        , roundtripTests
        , edgeCaseTests
        , strictParsingTests
        , parseUrlTests
        , modelToTypesTests
        , dictTests
        ]


basicTests : Test
basicTests =
    describe "basic"
        [ test "empty query string" <|
            \() ->
                Parser.parse noTypes ""
                    |> Expect.equal (Ok emptyQuery)
        , test "limit" <|
            \() ->
                Parser.parse noTypes "limit=1"
                    |> Expect.equal (Ok { emptyQuery | limit = Just 1 })
        , test "offset" <|
            \() ->
                Parser.parse noTypes "offset=5"
                    |> Expect.equal (Ok { emptyQuery | offset = Just 5 })
        , test "limit and offset" <|
            \() ->
                Parser.parse noTypes "limit=10&offset=20"
                    |> Expect.equal (Ok { emptyQuery | limit = Just 10, offset = Just 20 })
        ]


orderTests : Test
orderTests =
    describe "order"
        [ test "asc" <|
            \() ->
                Parser.parse noTypes "order%5Bid%5D=asc"
                    |> Expect.equal (Ok { emptyQuery | order = [ ( "id", Asc ) ] })
        , test "desc" <|
            \() ->
                Parser.parse noTypes "order%5Bid%5D=desc"
                    |> Expect.equal (Ok { emptyQuery | order = [ ( "id", Desc ) ] })
        , test "multiple orders" <|
            \() ->
                Parser.parse noTypes "order%5Bid%5D=asc&order%5Bname%5D=desc"
                    |> Expect.equal (Ok { emptyQuery | order = [ ( "id", Asc ), ( "name", Desc ) ] })
        , test "nested order column" <|
            \() ->
                Parser.parse noTypes "order%5Bsource_transaction%5D%5Bexecuted_at%5D=desc"
                    |> Expect.equal (Ok { emptyQuery | order = [ ( "source_transaction.executed_at", Desc ) ] })
        ]


predicateTests : Test
predicateTests =
    describe "predicate"
        [ test "eq string" <|
            \() ->
                Parser.parse noTypes "where%5Bname%5D%5Beq%5D=standardapi"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Eq [ "name" ] (StandardApi.String "standardapi")) })
        , test "eq int" <|
            \() ->
                Parser.parse noTypes "where%5Bid%5D%5Beq%5D=42"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Eq [ "id" ] (StandardApi.Int 42)) })
        , test "neq" <|
            \() ->
                Parser.parse noTypes "where%5Bname%5D%5Bnot_eq%5D=standardapi"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Neq [ "name" ] (StandardApi.String "standardapi")) })
        , test "lt" <|
            \() ->
                Parser.parse noTypes "where%5Bage%5D%5Blt%5D=30"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Lt [ "age" ] (StandardApi.Int 30)) })
        , test "lte" <|
            \() ->
                Parser.parse noTypes "where%5Bage%5D%5Blte%5D=30"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Lte [ "age" ] (StandardApi.Int 30)) })
        , test "gt" <|
            \() ->
                Parser.parse noTypes "where%5Bage%5D%5Bgt%5D=18"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Gt [ "age" ] (StandardApi.Int 18)) })
        , test "gte" <|
            \() ->
                Parser.parse noTypes "where%5Bage%5D%5Bgte%5D=18"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Gte [ "age" ] (StandardApi.Int 18)) })
        , test "ilike" <|
            \() ->
                Parser.parse noTypes "where%5Bname%5D%5Bilike%5D=%25api%25"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Ilike [ "name" ] (StandardApi.String "%api%")) })
        , test "null" <|
            \() ->
                Parser.parse noTypes "where%5Bdeleted_at%5D=false"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Null [ "deleted_at" ]) })
        , test "set" <|
            \() ->
                Parser.parse noTypes "where%5Bdeleted_at%5D=true"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Set [ "deleted_at" ]) })
        , test "in" <|
            \() ->
                Parser.parse noTypes "where%5Bid%5D%5B%5D=1&where%5Bid%5D%5B%5D=2&where%5Bid%5D%5B%5D=3"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (In [ "id" ] [ StandardApi.Int 1, StandardApi.Int 2, StandardApi.Int 3 ]) })
        , test "not_in" <|
            \() ->
                Parser.parse noTypes "where%5Bid%5D%5Bnot_in%5D%5B%5D=1&where%5Bid%5D%5Bnot_in%5D%5B%5D=2"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (NotIn [ "id" ] [ StandardApi.Int 1, StandardApi.Int 2 ]) })
        , test "overlaps" <|
            \() ->
                Parser.parse noTypes "where%5Btags%5D%5Boverlaps%5D%5B%5D=elm&where%5Btags%5D%5Boverlaps%5D%5B%5D=api"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Overlaps [ "tags" ] [ StandardApi.String "elm", StandardApi.String "api" ]) })
        , test "contains" <|
            \() ->
                Parser.parse noTypes "where%5Bregion_ids%5D%5Bcontains%5D=20106"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Contains [ "region_ids" ] (StandardApi.Int 20106)) })
        , test "float value" <|
            \() ->
                Parser.parse noTypes "where%5Bprice%5D%5Bgte%5D=9.99"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Gte [ "price" ] (StandardApi.Float 9.99)) })
        , test "bool value true" <|
            \() ->
                Parser.parse noTypes "where%5Bactive%5D%5Beq%5D=true"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Eq [ "active" ] (StandardApi.Bool True)) })
        , test "bool value false" <|
            \() ->
                Parser.parse noTypes "where%5Bactive%5D%5Beq%5D=false"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Eq [ "active" ] (StandardApi.Bool False)) })
        , test "conjunction" <|
            \() ->
                Parser.parse noTypes "where%5B%5D%5Bname%5D%5Beq%5D=standardapi&where%5B%5D%5Bversion%5D%5Beq%5D=1"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | predicate =
                                    Just
                                        (Conjunction
                                            (Eq [ "name" ] (StandardApi.String "standardapi"))
                                            (Eq [ "version" ] (StandardApi.Int 1))
                                        )
                            }
                        )
        , test "conjunction id and name" <|
            \() ->
                Parser.parse noTypes "where%5B%5D%5Bid%5D%5Beq%5D=1&where%5B%5D%5Bname%5D%5Beq%5D=Test"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | predicate =
                                    Just
                                        (Conjunction
                                            (Eq [ "id" ] (StandardApi.Int 1))
                                            (Eq [ "name" ] (StandardApi.String "Test"))
                                        )
                            }
                        )
        , test "disjunction" <|
            \() ->
                Parser.parse noTypes "where%5B%5D%5Bname%5D%5Beq%5D=foo&where%5B%5D=OR&where%5B%5D%5Bname%5D%5Beq%5D=bar"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | predicate =
                                    Just
                                        (Disjunction
                                            (Eq [ "name" ] (StandardApi.String "foo"))
                                            (Eq [ "name" ] (StandardApi.String "bar"))
                                        )
                            }
                        )
        , test "nested column path" <|
            \() ->
                Parser.parse noTypes "where%5Bmetadata%5D%5Bkey%5D%5Beq%5D=name"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Eq [ "metadata", "key" ] (StandardApi.String "name")) })
        ]


schemaDecodingTests : Test
schemaDecodingTests =
    describe "schema-driven decoding"
        [ test "int attribute decoded as Int" <|
            \() ->
                Parser.parse typedColumns "where%5Bid%5D%5Beq%5D=42"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Eq [ "id" ] (StandardApi.Int 42)) })
        , test "string attribute keeps numeric string as String" <|
            \() ->
                Parser.parse typedColumns "where%5Bname%5D%5Beq%5D=123"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Eq [ "name" ] (StandardApi.String "123")) })
        , test "float attribute decoded as Float" <|
            \() ->
                Parser.parse typedColumns "where%5Bprice%5D%5Bgte%5D=9.99"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Gte [ "price" ] (StandardApi.Float 9.99)) })
        , test "float attribute decodes integer string as Float" <|
            \() ->
                Parser.parse typedColumns "where%5Bprice%5D%5Bgte%5D=10"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Gte [ "price" ] (StandardApi.Float 10.0)) })
        , test "bool attribute decoded as Bool" <|
            \() ->
                Parser.parse typedColumns "where%5Bactive%5D%5Beq%5D=true"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Eq [ "active" ] (StandardApi.Bool True)) })
        , test "posix attribute decoded as Posix" <|
            \() ->
                Parser.parse typedColumns "where%5Bcreated_at%5D%5Bgte%5D=1970-01-01T00%3A00%3A00.000Z"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | predicate =
                                    Just (Gte [ "created_at" ] (StandardApi.Posix (Time.millisToPosix 0)))
                            }
                        )
        , test "unknown attribute falls back to guessing" <|
            \() ->
                Parser.parse typedColumns "where%5Bunknown%5D%5Beq%5D=42"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Eq [ "unknown" ] (StandardApi.Int 42)) })
        , test "int attribute with non-numeric value returns error" <|
            \() ->
                Parser.parse typedColumns "where%5Bid%5D%5Beq%5D=abc"
                    |> Expect.err
        , test "float attribute with non-numeric value returns error" <|
            \() ->
                Parser.parse typedColumns "where%5Bprice%5D%5Bgte%5D=abc"
                    |> Expect.err
        , test "bool attribute with non-boolean value returns error" <|
            \() ->
                Parser.parse typedColumns "where%5Bactive%5D%5Beq%5D=maybe"
                    |> Expect.err
        , test "posix attribute with invalid date returns error" <|
            \() ->
                Parser.parse typedColumns "where%5Bcreated_at%5D%5Bgte%5D=not-a-date"
                    |> Expect.err
        , test "conjunction with mixed types uses schema" <|
            \() ->
                Parser.parse typedColumns "where%5B%5D%5Bid%5D%5Beq%5D=1&where%5B%5D%5Bname%5D%5Beq%5D=1"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | predicate =
                                    Just
                                        (Conjunction
                                            (Eq [ "id" ] (StandardApi.Int 1))
                                            (Eq [ "name" ] (StandardApi.String "1"))
                                        )
                            }
                        )
        ]


includeTests : Test
includeTests =
    describe "include"
        [ test "simple" <|
            \() ->
                Parser.parse noTypes "include%5Bauthor%5D=true"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | includes =
                                    [ tree (include ( "author", emptyQuery )) [] ]
                            }
                        )
        , test "multiple" <|
            \() ->
                Parser.parse noTypes "include%5Bauthor%5D=true&include%5Bcontributors%5D=true"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | includes =
                                    [ tree (include ( "author", emptyQuery )) []
                                    , tree (include ( "contributors", emptyQuery )) []
                                    ]
                            }
                        )
        , test "with limit" <|
            \() ->
                Parser.parse noTypes "include%5Bcontributors%5D%5Blimit%5D=1"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | includes =
                                    [ tree (include ( "contributors", { emptyQuery | limit = Just 1 } )) [] ]
                            }
                        )
        , test "with offset" <|
            \() ->
                Parser.parse noTypes "include%5Bcontributors%5D%5Boffset%5D=1"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | includes =
                                    [ tree (include ( "contributors", { emptyQuery | offset = Just 1 } )) [] ]
                            }
                        )
        , test "with order" <|
            \() ->
                Parser.parse noTypes "include%5Bcontributors%5D%5Border%5D%5Bid%5D=asc"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | includes =
                                    [ tree (include ( "contributors", { emptyQuery | order = [ ( "id", Asc ) ] } )) [] ]
                            }
                        )
        , test "with where" <|
            \() ->
                Parser.parse noTypes "include%5Bcontributors%5D%5Bwhere%5D%5Bname%5D%5Beq%5D=elon"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | includes =
                                    [ tree
                                        (include
                                            ( "contributors"
                                            , { emptyQuery | predicate = Just (Eq [ "name" ] (StandardApi.String "elon")) }
                                            )
                                        )
                                        []
                                    ]
                            }
                        )
        , test "with where conjunction" <|
            \() ->
                Parser.parse noTypes "include%5Bcontributors%5D%5Bwhere%5D%5B%5D%5Bname%5D%5Beq%5D=elon&include%5Bcontributors%5D%5Bwhere%5D%5B%5D%5Borg%5D%5Beq%5D=tesla"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | includes =
                                    [ tree
                                        (include
                                            ( "contributors"
                                            , { emptyQuery
                                                | predicate =
                                                    Just
                                                        (Conjunction
                                                            (Eq [ "name" ] (StandardApi.String "elon"))
                                                            (Eq [ "org" ] (StandardApi.String "tesla"))
                                                        )
                                              }
                                            )
                                        )
                                        []
                                    ]
                            }
                        )
        , test "nested" <|
            \() ->
                Parser.parse noTypes "include%5Bauthor%5D%5Borganization%5D=true"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | includes =
                                    [ tree
                                        (include ( "author", emptyQuery ))
                                        [ tree (include ( "organization", emptyQuery )) [] ]
                                    ]
                            }
                        )
        , test "nested with subquery" <|
            \() ->
                Parser.parse noTypes "include%5Bauthor%5D%5Blimit%5D=1&include%5Bauthor%5D%5Borganization%5D=true"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | includes =
                                    [ tree
                                        (include ( "author", { emptyQuery | limit = Just 1 } ))
                                        [ tree (include ( "organization", emptyQuery )) [] ]
                                    ]
                            }
                        )
        ]


roundtripTests : Test
roundtripTests =
    describe "roundtrip"
        [ test "empty query" <|
            \() ->
                roundtrip emptyQuery
        , test "with limit" <|
            \() ->
                roundtrip { emptyQuery | limit = Just 5 }
        , test "with offset" <|
            \() ->
                roundtrip { emptyQuery | offset = Just 10 }
        , test "with order asc" <|
            \() ->
                roundtrip { emptyQuery | order = [ ( "created_at", Asc ) ] }
        , test "with order desc" <|
            \() ->
                roundtrip { emptyQuery | order = [ ( "created_at", Desc ) ] }
        , test "with eq int" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (Eq [ "id" ] (StandardApi.Int 42)) }
        , test "with eq string" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (Eq [ "name" ] (StandardApi.String "test")) }
        , test "with neq" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (Neq [ "name" ] (StandardApi.String "bad")) }
        , test "with lt" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (Lt [ "age" ] (StandardApi.Int 30)) }
        , test "with lte" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (Lte [ "age" ] (StandardApi.Int 30)) }
        , test "with gt" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (Gt [ "age" ] (StandardApi.Int 18)) }
        , test "with gte" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (Gte [ "age" ] (StandardApi.Int 18)) }
        , test "with ilike" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (Ilike [ "name" ] (StandardApi.String "%api%")) }
        , test "with contains" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (Contains [ "region_ids" ] (StandardApi.Int 20106)) }
        , test "with null" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (Null [ "deleted_at" ]) }
        , test "with set" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (Set [ "deleted_at" ]) }
        , test "with not_in" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (NotIn [ "id" ] [ StandardApi.Int 1, StandardApi.Int 2 ]) }
        , test "with overlaps" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (Overlaps [ "tags" ] [ StandardApi.String "elm", StandardApi.String "api" ]) }
        , test "with in" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (In [ "id" ] [ StandardApi.Int 1, StandardApi.Int 2, StandardApi.Int 3 ]) }
        , test "with conjunction" <|
            \() ->
                roundtrip
                    { emptyQuery
                        | predicate =
                            Just
                                (Conjunction
                                    (Eq [ "id" ] (StandardApi.Int 1))
                                    (Eq [ "name" ] (StandardApi.String "Test"))
                                )
                    }
        , test "with disjunction" <|
            \() ->
                roundtrip
                    { emptyQuery
                        | predicate =
                            Just
                                (Disjunction
                                    (Eq [ "name" ] (StandardApi.String "foo"))
                                    (Eq [ "name" ] (StandardApi.String "bar"))
                                )
                    }
        , test "with float value" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (Gte [ "price" ] (StandardApi.Float 9.99)) }
        , test "with bool true value" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (Eq [ "active" ] (StandardApi.Bool True)) }
        , test "with bool false value" <|
            \() ->
                roundtrip { emptyQuery | predicate = Just (Eq [ "active" ] (StandardApi.Bool False)) }
        , test "with simple include" <|
            \() ->
                roundtrip
                    { emptyQuery
                        | includes = [ tree (include ( "author", emptyQuery )) [] ]
                    }
        , test "with multiple includes" <|
            \() ->
                roundtrip
                    { emptyQuery
                        | includes =
                            [ tree (include ( "author", emptyQuery )) []
                            , tree (include ( "contributors", emptyQuery )) []
                            ]
                    }
        , test "with include with limit" <|
            \() ->
                roundtrip
                    { emptyQuery
                        | includes = [ tree (include ( "users", { emptyQuery | limit = Just 3 } )) [] ]
                    }
        , test "with include with offset" <|
            \() ->
                roundtrip
                    { emptyQuery
                        | includes = [ tree (include ( "users", { emptyQuery | offset = Just 5 } )) [] ]
                    }
        , test "with include with order" <|
            \() ->
                roundtrip
                    { emptyQuery
                        | includes = [ tree (include ( "users", { emptyQuery | order = [ ( "id", Asc ) ] } )) [] ]
                    }
        , test "with include with where" <|
            \() ->
                roundtrip
                    { emptyQuery
                        | includes =
                            [ tree
                                (include
                                    ( "users"
                                    , { emptyQuery | predicate = Just (Eq [ "name" ] (StandardApi.String "test")) }
                                    )
                                )
                                []
                            ]
                    }
        , test "with nested include" <|
            \() ->
                roundtrip
                    { emptyQuery
                        | includes =
                            [ tree
                                (include ( "author", emptyQuery ))
                                [ tree (include ( "organization", emptyQuery )) [] ]
                            ]
                    }
        , test "with nested include via includes field parses as tree children" <|
            \() ->
                let
                    input =
                        { emptyQuery
                            | includes =
                                [ tree
                                    (include
                                        ( "author"
                                        , { emptyQuery
                                            | includes =
                                                [ tree (include ( "organization", emptyQuery )) [] ]
                                          }
                                        )
                                    )
                                    []
                                ]
                        }

                    expected =
                        { emptyQuery
                            | includes =
                                [ tree
                                    (include ( "author", emptyQuery ))
                                    [ tree (include ( "organization", emptyQuery )) [] ]
                                ]
                        }

                    url =
                        Builder.absolute [ "test" ] input

                    queryString =
                        case String.split "?" url of
                            [ _, qs ] ->
                                qs

                            _ ->
                                ""
                in
                Parser.parse noTypes queryString
                    |> Expect.equal (Ok expected)
        , test "with complex query" <|
            \() ->
                roundtrip
                    { limit = Just 10
                    , offset = Just 20
                    , order = [ ( "created_at", Desc ) ]
                    , predicate = Just (Eq [ "status" ] (StandardApi.String "active"))
                    , includes =
                        [ tree (include ( "author", emptyQuery )) []
                        , tree (include ( "tags", { emptyQuery | limit = Just 5 } )) []
                        ]
                    }
        ]


edgeCaseTests : Test
edgeCaseTests =
    describe "edge cases"
        [ test "value containing equals sign" <|
            \() ->
                Parser.parse noTypes "where%5Btoken%5D%5Beq%5D=abc%3Ddef"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Eq [ "token" ] (StandardApi.String "abc=def")) })
        , test "error result type is Result" <|
            \() ->
                case Parser.parse noTypes "" of
                    Ok q ->
                        q |> Expect.equal emptyQuery

                    Err _ ->
                        Expect.fail "should not fail on empty string"
        , test "empty key is ignored" <|
            \() ->
                Parser.parse noTypes "=value"
                    |> Expect.equal (Ok emptyQuery)
        , test "key with empty value" <|
            \() ->
                Parser.parse noTypes "where%5Bname%5D%5Beq%5D="
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Eq [ "name" ] (StandardApi.String "")) })
        , test "key with no equals treated as empty value" <|
            \() ->
                Parser.parse noTypes "limit"
                    |> Expect.err
        ]


{-| Test that building a URL and parsing it back gives the same Query.
Uses no type mappings so the parser falls back to heuristic guessing,
which matches how the builder serializes values.
-}
roundtrip : Query -> Expect.Expectation
roundtrip query =
    let
        url =
            Builder.absolute [ "test" ] query

        queryString =
            case String.split "?" url of
                [ _, qs ] ->
                    qs

                _ ->
                    ""
    in
    Parser.parse noTypes queryString
        |> Expect.equal (Ok query)


typedRoundtrip : List ( String, Type.Type ) -> Query -> Expect.Expectation
typedRoundtrip types query =
    let
        url =
            Builder.absolute [ "test" ] query

        queryString =
            case String.split "?" url of
                [ _, qs ] ->
                    qs

                _ ->
                    ""
    in
    Parser.parse types queryString
        |> Expect.equal (Ok query)


strictParsingTests : Test
strictParsingTests =
    describe "strict parsing"
        [ test "garbage limit returns error" <|
            \() ->
                Parser.parse noTypes "limit=abc"
                    |> Expect.err
        , test "garbage offset returns error" <|
            \() ->
                Parser.parse noTypes "offset=abc"
                    |> Expect.err
        , test "invalid order direction returns error" <|
            \() ->
                Parser.parse noTypes "order%5Bid%5D=sideways"
                    |> Expect.err
        , test "limit error message is descriptive" <|
            \() ->
                Parser.parse noTypes "limit=abc"
                    |> Expect.equal (Err "expected integer for limit, got: abc")
        , test "offset error message is descriptive" <|
            \() ->
                Parser.parse noTypes "offset=xyz"
                    |> Expect.equal (Err "expected integer for offset, got: xyz")
        , test "order error message is descriptive" <|
            \() ->
                Parser.parse noTypes "order%5Bid%5D=sideways"
                    |> Expect.equal (Err "expected asc or desc for order[id], got: sideways")
        , test "posix roundtrip with typed columns" <|
            \() ->
                typedRoundtrip typedColumns
                    { emptyQuery
                        | predicate =
                            Just (Gte [ "created_at" ] (StandardApi.Posix (Time.millisToPosix 0)))
                    }
        , test "int type mismatch error message" <|
            \() ->
                Parser.parse typedColumns "where%5Bid%5D%5Beq%5D=abc"
                    |> Expect.equal (Err "expected integer for id, got: abc")
        , test "sub-include predicate error propagates" <|
            \() ->
                Parser.parse typedColumns "include%5Bauthor%5D%5Bwhere%5D%5Bid%5D%5Beq%5D=abc"
                    |> Expect.err
        , test "sub-include garbage limit propagates error" <|
            \() ->
                Parser.parse noTypes "include%5Bauthor%5D%5Blimit%5D=abc"
                    |> Expect.err
        ]


parseUrlTests : Test
parseUrlTests =
    describe "parseUrl"
        [ test "parses query from Url" <|
            \() ->
                let
                    url =
                        { protocol = Url.Https
                        , host = "example.com"
                        , port_ = Nothing
                        , path = "/test"
                        , query = Just "limit=5"
                        , fragment = Nothing
                        }
                in
                Parser.parseUrl noTypes url
                    |> Expect.equal (Ok { emptyQuery | limit = Just 5 })
        , test "empty query in Url" <|
            \() ->
                let
                    url =
                        { protocol = Url.Https
                        , host = "example.com"
                        , port_ = Nothing
                        , path = "/test"
                        , query = Nothing
                        , fragment = Nothing
                        }
                in
                Parser.parseUrl noTypes url
                    |> Expect.equal (Ok emptyQuery)
        ]


modelToTypesTests : Test
modelToTypesTests =
    describe "modelToTypes"
        [ test "converts model attributes to type list" <|
            \() ->
                let
                    model =
                        { name = "test"
                        , attributes =
                            [ { name = "id", type_ = Type.Int, default = Nothing, primaryKey = True, null = False, array = False, comment = "" }
                            , { name = "name", type_ = Type.String, default = Nothing, primaryKey = False, null = False, array = False, comment = "" }
                            ]
                        , comment = ""
                        }
                in
                modelToTypes model
                    |> Expect.equal [ ( "id", Type.Int ), ( "name", Type.String ) ]
        , test "empty model gives empty list" <|
            \() ->
                modelToTypes { name = "empty", attributes = [], comment = "" }
                    |> Expect.equal []
        , test "works with parse" <|
            \() ->
                let
                    model =
                        { name = "test"
                        , attributes =
                            [ { name = "id", type_ = Type.Int, default = Nothing, primaryKey = True, null = False, array = False, comment = "" }
                            ]
                        , comment = ""
                        }
                in
                Parser.parse (modelToTypes model) "where%5Bid%5D%5Beq%5D=42"
                    |> Expect.equal (Ok { emptyQuery | predicate = Just (Eq [ "id" ] (StandardApi.Int 42)) })
        ]


dictTests : Test
dictTests =
    describe "dict values"
        [ test "single key dict" <|
            \() ->
                -- where[metadata][eq][key]=value -> Eq ["metadata"] (Dict {"key": String "value"})
                Parser.parse noTypes "where%5Bmetadata%5D%5Beq%5D%5Bkey%5D=value"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | predicate =
                                    Just (Eq [ "metadata" ] (Dict (Dict.singleton "key" (StandardApi.String "value"))))
                            }
                        )
        , test "multi key dict" <|
            \() ->
                -- where[metadata][eq][k1]=v1&where[metadata][eq][k2]=v2
                Parser.parse noTypes "where%5B%5D%5Bmetadata%5D%5Beq%5D%5Bk1%5D=v1&where%5B%5D%5Bmetadata%5D%5Beq%5D%5Bk2%5D=v2"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | predicate =
                                    Just
                                        (Eq [ "metadata" ]
                                            (Dict
                                                (Dict.fromList
                                                    [ ( "k1", StandardApi.String "v1" )
                                                    , ( "k2", StandardApi.String "v2" )
                                                    ]
                                                )
                                            )
                                        )
                            }
                        )
        , test "dict roundtrip single key" <|
            \() ->
                roundtrip
                    { emptyQuery
                        | predicate =
                            Just (Eq [ "metadata" ] (Dict (Dict.singleton "key" (StandardApi.String "value"))))
                    }
        , test "dict roundtrip multi key" <|
            \() ->
                roundtrip
                    { emptyQuery
                        | predicate =
                            Just
                                (Eq [ "metadata" ]
                                    (Dict
                                        (Dict.fromList
                                            [ ( "k1", StandardApi.String "v1" )
                                            , ( "k2", StandardApi.String "v2" )
                                            ]
                                        )
                                    )
                                )
                    }
        , test "dict with int value" <|
            \() ->
                Parser.parse noTypes "where%5Bmetadata%5D%5Beq%5D%5Bcount%5D=42"
                    |> Expect.equal
                        (Ok
                            { emptyQuery
                                | predicate =
                                    Just (Eq [ "metadata" ] (Dict (Dict.singleton "count" (StandardApi.Int 42))))
                            }
                        )
        ]
