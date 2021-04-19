module Url.BuilderTest exposing (..)

import Expect
import Http exposing (Expect)
import StandardApi exposing (..)
import StandardApi.Url.Builder exposing (..)
import Test exposing (..)
import Tree exposing (label, tree)


absoluteTest : Test
absoluteTest =
    describe "StandardApi.Url.Builder"
        [ test "URL" <|
            \() ->
                absolute [ "packages" ] emptyQuery
                    |> Expect.equal "/packages"
        , test "URL with empty query" <|
            \() ->
                absolute [ "packages" ] emptyQuery
                    |> Expect.equal "/packages"
        , test "URL with limit" <|
            \() ->
                absolute [ "packages" ] { emptyQuery | limit = Just 1 }
                    |> Expect.equal "/packages?limit=1"
        , test "URL with offset" <|
            \() ->
                absolute [ "packages" ] { emptyQuery | offset = Just 1 }
                    |> Expect.equal "/packages?offset=1"
        , test "URL with order asc" <|
            \() ->
                absolute [ "packages" ] { emptyQuery | order = [ ( "id", Asc ) ] }
                    |> Expect.equal "/packages?order%5Bid%5D=asc"
        , test "URL with order desc" <|
            \() ->
                absolute [ "packages" ] { emptyQuery | order = [ ( "id", Desc ) ] }
                    |> Expect.equal "/packages?order%5Bid%5D=desc"
        , test "URL with where" <|
            \() ->
                absolute [ "packages" ] { emptyQuery | wheres = [ ( "name", Eq (StandardApi.String "standardapi") ) ] }
                    |> Expect.equal "/packages?where%5Bname%5D%5Beq%5D=standardapi"
        , test "URL with wheres" <|
            \() ->
                absolute [ "packages" ]
                    { emptyQuery
                        | wheres =
                            [ ( "name", Eq (StandardApi.String "standardapi") )
                            , ( "version", Eq (StandardApi.Int 1) )
                            ]
                    }
                    |> Expect.equal "/packages?where%5Bname%5D%5Beq%5D=standardapi&where%5Bversion%5D%5Beq%5D=1"
        , test "URL with include" <|
            \() ->
                absolute [ "packages" ]
                    { emptyQuery
                        | includes =
                            [ tree (include ( "author", emptyQuery )) []
                            ]
                    }
                    |> Expect.equal "/packages?include%5Bauthor%5D=true"
        , test "URL with includes" <|
            \() ->
                absolute [ "packages" ]
                    { emptyQuery
                        | includes =
                            [ Tree.tree (StandardApi.include ( "author", emptyQuery )) []
                            , Tree.tree (StandardApi.include ( "contributors", emptyQuery )) []
                            ]
                    }
                    |> Expect.equal "/packages?include%5Bauthor%5D=true&include%5Bcontributors%5D=true"
        , test "URL with include with limit" <|
            \() ->
                absolute [ "packages" ]
                    { emptyQuery
                        | includes =
                            [ tree (include ( "contributors", { emptyQuery | limit = Just 1 } )) []
                            ]
                    }
                    |> Expect.equal "/packages?include%5Bcontributors%5D%5Blimit%5D=1"
        , test "URL with include with offset" <|
            \() ->
                absolute [ "packages" ]
                    { emptyQuery
                        | includes =
                            [ tree (include ( "contributors", { emptyQuery | offset = Just 1 } )) []
                            ]
                    }
                    |> Expect.equal "/packages?include%5Bcontributors%5D%5Boffset%5D=1"
        , test "URL with include order" <|
            \() ->
                absolute [ "packages" ]
                    { emptyQuery
                        | includes =
                            [ tree (include ( "contributors", { emptyQuery | order = [ ( "id", Asc ) ] } )) []
                            ]
                    }
                    |> Expect.equal "/packages?include%5Bcontributors%5D%5Border%5D%5Bid%5D=asc"
        , test "URL with include where" <|
            \() ->
                absolute [ "packages" ]
                    { emptyQuery
                        | includes =
                            [ tree
                                (include
                                    ( "contributors"
                                    , { emptyQuery
                                        | wheres =
                                            [ ( "name", Eq (StandardApi.String "elon") ) ]
                                      }
                                    )
                                )
                                []
                            ]
                    }
                    |> Expect.equal "/packages?include%5Bcontributors%5D%5Bwhere%5D%5Bname%5D%5Beq%5D=elon"
        , test "URL with include wheres" <|
            \() ->
                absolute [ "packages" ]
                    { emptyQuery
                        | includes =
                            [ tree
                                (include
                                    ( "contributors"
                                    , { emptyQuery
                                        | wheres =
                                            [ ( "name", Eq (StandardApi.String "elon") )
                                            , ( "organization", Eq (StandardApi.String "tesla") )
                                            ]
                                      }
                                    )
                                )
                                []
                            ]
                    }
                    |> Expect.equal "/packages?include%5Bcontributors%5D%5Bwhere%5D%5Bname%5D%5Beq%5D=elon&include%5Bcontributors%5D%5Bwhere%5D%5Borganization%5D%5Beq%5D=tesla"
        , test "URL with include include" <|
            \() ->
                absolute [ "packages" ]
                    { emptyQuery
                        | includes =
                            [ tree
                                (include
                                    ( "author"
                                    , { emptyQuery
                                        | includes =
                                            [ tree
                                                (include
                                                    ( "organization"
                                                    , emptyQuery
                                                    )
                                                )
                                                []
                                            ]
                                      }
                                    )
                                )
                                []
                            ]
                    }
                    |> Expect.equal "/packages?include%5Bauthor%5D%5Borganization%5D=true"
        ]
