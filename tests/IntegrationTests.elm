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
        ((test "Example works to generate decoder for" <|
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
                    Err e ->
                        Expect.fail ("Failed to parse source file" ++ Debug.toString e)

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
         )
            :: (cases
                    |> List.map
                        (\t ->
                            test ("Creating decoder and encoder from " ++ t ++ " works") <|
                                \_ ->
                                    let
                                        rawFile =
                                            Parser.parse (baseModule t)
                                    in
                                    case rawFile of
                                        Err e ->
                                            Expect.fail ("Failed to parse source file" ++ Debug.toString e)

                                        Ok f ->
                                            Main.generateDecodersAndEncoders (Dependency.InModule [ "Test" ])
                                                "Foo"
                                                { typesToGenerate = [ ( InModule [ "Test" ], "Test" ) ]
                                                , rawFiles = Dict.fromList [ ( [ "Test" ], f ) ]
                                                , filesContent = Dict.empty
                                                , generatedTypes = []
                                                }
                                                |> (Tuple.first >> .filesContent >> Dict.get [ "Test" ])
                                                |> (\res ->
                                                        case res of
                                                            Just { decoders, encoders } ->
                                                                case
                                                                    (decoders ++ encoders)
                                                                        |> List.filter
                                                                            (\ff ->
                                                                                String.contains "UNHANDLED TYPE" ff
                                                                            )
                                                                of
                                                                    [] ->
                                                                        Expect.pass

                                                                    l ->
                                                                        Expect.fail ("Decoder or encoder had UNHANDLED types:\n " ++ Debug.toString l)

                                                            Nothing ->
                                                                Expect.fail "Failed to generate encoders and decoders!"
                                                   )
                        )
               )
        )


types : List String
types =
    [ "Bool", "String", "Int", "Float" ]


containers : List String
containers =
    [ "Set", "Array", "List" ]


multiTypeContainers : List String
multiTypeContainers =
    [ "Dict" ]


multiTypeContainerValidTypes : List String
multiTypeContainerValidTypes =
    [ "String", "Int", "Float" ]


otherTypes : List String
otherTypes =
    [ "Time.Posix" ]


cases : List String
cases =
    types
        ++ List.concatMap (\c -> List.map (\t -> c ++ " " ++ t) types) containers
        ++ List.concatMap (\c -> List.map (\t -> "Dict (" ++ c ++ " " ++ t ++ ") " ++ t) types) containers
        ++ List.concatMap (\c -> List.map (\t -> c ++ " " ++ t ++ " " ++ t) multiTypeContainerValidTypes) multiTypeContainers
        ++ List.map (\t -> "Maybe " ++ t) types
        ++ List.concatMap (\c -> List.map (\t -> "Maybe (" ++ c ++ " " ++ t ++ ")") types) containers
        ++ otherTypes


baseModule : String -> String
baseModule t =
    "module Test exposing (..)\n\ntype alias Foo = { a : " ++ t ++ " }"
