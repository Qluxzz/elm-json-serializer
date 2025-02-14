module Generator exposing (..)

import Dependency exposing (DecodersEncodersDeps)
import Elm.Syntax.Declaration as Declaration
import Generator.Decoder
import Generator.Encoder


generateDecodersEncodersAndDeps : Declaration.Declaration -> Maybe DecodersEncodersDeps
generateDecodersEncodersAndDeps declaration =
    case declaration of
        Declaration.AliasDeclaration decl ->
            let
                ( decoder, deps ) =
                    Generator.Decoder.generateAliasDecoderAndDeps decl

                encoder =
                    Generator.Encoder.generateAliasEncoder decl
            in
            Just
                { decoder = decoder
                , encoder = encoder
                , dependencies = deps
                }

        Declaration.CustomTypeDeclaration decl ->
            let
                ( decoder, deps ) =
                    Generator.Decoder.generateTypedDecoderAndDeps decl

                encoder =
                    Generator.Encoder.generateTypedEncoder decl
            in
            Just
                { decoder = decoder
                , encoder = encoder
                , dependencies = deps
                }

        _ ->
            Nothing
