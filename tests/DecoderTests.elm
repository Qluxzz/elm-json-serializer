module DecoderTests exposing (..)

import Dependency exposing (Dependency)
import Elm.Syntax.Node as Node
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Expect
import Generator.Decoder
import Test exposing (Test, describe, test)


typeCases : List ( ( String, List (Node.Node TypeAnnotation) ), ( String, List ( Dependency, String ) ) )
typeCases =
    [ ( ( "String", [] ), ( "Decode.string", [] ) )
    , ( ( "Bool", [] ), ( "Decode.bool", [] ) )
    , ( ( "Int", [] ), ( "Decode.int", [] ) )
    , ( ( "Float", [] ), ( "Decode.float", [] ) )
    ]


suite : Test
suite =
    describe "Decoder tests"
        [ describe "Decodes type correctly"
            (List.map
                (\( ( type_, annotations ), expected ) ->
                    test type_ <|
                        \_ ->
                            Generator.Decoder.genericTypeDecoder type_ annotations
                                |> Expect.equal expected
                )
                typeCases
            )
        ]
