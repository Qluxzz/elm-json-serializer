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
                                        ( [ "decodeFoo : Json.Decode.Decoder Foo\ndecodeFoo =\n (Json.Decode.succeed Foo \n|> andMap (Json.Decode.field \"a\" (Json.Decode.int))|> andMap (Json.Decode.field \"b\" (Json.Decode.list(Json.Decode.string)))|> andMap (Json.Decode.field \"c\" (Json.Decode.bool)))"
                                          , "decodeTest : Json.Decode.Decoder Test\ndecodeTest =\n (Json.Decode.succeed Test \n|> andMap (Json.Decode.field \"foo\" (Json.Decode.int))|> andMap (Json.Decode.field \"bar\" (Json.Decode.string))|> andMap (Json.Decode.field \"biz\" (Json.Decode.dict(decodeFoo))))"
                                          ]
                                        , [ "encodeFoo : Foo -> Json.Encode.Value\nencodeFoo record =\n (Json.Encode.object [(\"a\", (Json.Encode.int record.a))\n, (\"b\", (Json.Encode.list(Json.Encode.string) record.b))\n, (\"c\", (Json.Encode.bool record.c))])"
                                          , "encodeTest : Test -> Json.Encode.Value\nencodeTest record =\n (Json.Encode.object [(\"foo\", (Json.Encode.int record.foo))\n, (\"bar\", (Json.Encode.string record.bar))\n, (\"biz\", (Json.Encode.dict identity (encodeFoo) record.biz))])"
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
