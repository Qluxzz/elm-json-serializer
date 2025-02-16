module IntegrationTests exposing (..)

import Dependency exposing (Dependency(..))
import Dict
import Elm.Parser as Parser
import Expect
import Main
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "E2E tests"
        [ test "Example works to generate decoder for" <|
            \_ ->
                let
                    rawFile =
                        Parser.parse """
module Test exposing (..)

import Dict exposing (Dict)

type alias Foo =
    { a : Int
    , b : List String
    , c : Bool
    }


type alias Test =
    { foo : Int
    , bar : String
    , biz : Dict String Foo
    }
"""
                in
                case rawFile of
                    Err _ ->
                        Expect.fail "Failed to parse source file"

                    Ok f ->
                        Main.generateDecodersAndEncoders (Dependency.InModule [ "Test" ])
                            "Foo"
                            { typesToGenerate = [ ( InModule [ "Test" ], "Foo" ), ( InModule [ "Test" ], "Test" ) ]
                            , rawFiles = Dict.fromList [ ( [ "Test" ], f ) ]
                            , filesContent = Dict.empty
                            , generatedTypes = []
                            }
                            |> (Tuple.first >> .filesContent >> Dict.get [ "Test" ])
                            |> Maybe.map
                                (\{ decoders, encoders } ->
                                    Expect.equal
                                        ( [ "decodeFoo : Decoder Foo\ndecodeFoo =\n (Decode.succeed Foo \n|> andMap (Decode.field \"a\" (Decode.int))|> andMap (Decode.field \"b\" (Decode.list(Decode.string)))|> andMap (Decode.field \"c\" (Decode.bool)))"
                                          , "decodeTest : Decoder Test\ndecodeTest =\n (Decode.succeed Test \n|> andMap (Decode.field \"foo\" (Decode.int))|> andMap (Decode.field \"bar\" (Decode.string))|> andMap (Decode.field \"biz\" (Decode.dict(decodeFoo))))"
                                          ]
                                        , [ "encodeFoo : Foo -> Encode.Value\nencodeFoo record =\n (Encode.object [(\"a\", (Encode.int record.a))\n, (\"b\", (Encode.list(Encode.string) record.b))\n, (\"c\", (Encode.bool record.c))])"
                                          , "encodeTest : Test -> Encode.Value\nencodeTest record =\n (Encode.object [(\"foo\", (Encode.int record.foo))\n, (\"bar\", (Encode.string record.bar))\n, (\"biz\", (Encode.dict identity (encodeFoo) record.biz))])"
                                          ]
                                        )
                                        ( decoders
                                        , encoders
                                        )
                                )
                            |> Maybe.withDefault (Expect.fail "Failed to get data")
        ]
