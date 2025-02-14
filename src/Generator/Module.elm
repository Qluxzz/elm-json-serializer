module Generator.Module exposing (..)

import Declaration
import Dependency exposing (Dependency(..))
import Dict exposing (Dict)
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Generator.Static
import String.Extra


type DecoderEncoder
    = Decoder
    | Encoder


addModuleName :
    Dict ModuleName RawFile
    -> List ( Dependency, String )
    -> ModuleName
    -> DecoderEncoder
    -> String
    -> String
addModuleName rawFiles dependencies moduleName decoder content =
    let
        imports =
            dependencies
                |> List.concatMap
                    (generateImportsFromDeps rawFiles moduleName decoder)
                |> String.Extra.newlineJoin
    in
    generateFileContent moduleName imports content <|
        case decoder of
            Decoder ->
                GenerationRequirements
                    "Decoder"
                    "Json.Decode as Decode exposing (Decoder)"

            Encoder ->
                GenerationRequirements
                    "Encoder"
                    "Json.Encode as Encode"


type alias GenerationRequirements =
    { moduleNamespace : String
    , imported : String
    }


generateImportsFromDeps :
    Dict ModuleName RawFile
    -> ModuleName
    -> DecoderEncoder
    -> ( Dependency, String )
    -> List String
generateImportsFromDeps rawFiles moduleName decoder ( dependency, typeName ) =
    case dependency of
        InModule name ->
            if name == moduleName then
                []

            else
                importDependency name decoder

        InOneOf names ->
            names
                |> List.map (\name -> ( name, Dict.get name rawFiles ))
                |> List.map (getDeclarationsInRawFiles typeName)
                |> List.concatMap (validDeclarationToImport decoder)


getDeclarationsInRawFiles :
    String
    -> ( ModuleName, Maybe RawFile )
    -> ( ModuleName, Maybe Declaration.Declaration )
getDeclarationsInRawFiles typeName =
    Tuple.mapSecond <|
        Maybe.andThen
            (Declaration.getDeclarationByName typeName)


validDeclarationToImport :
    DecoderEncoder
    -> ( ModuleName, Maybe Declaration.Declaration )
    -> List String
validDeclarationToImport decoder ( name, declaration ) =
    case declaration of
        Nothing ->
            []

        Just _ ->
            importDependency name decoder


importDependency : ModuleName -> DecoderEncoder -> List String
importDependency name decoder =
    [ "import"
    , String.join "." name
        ++ "."
        ++ (case decoder of
                Decoder ->
                    "Decoder"

                Encoder ->
                    "Encoder"
           )
    , "exposing (..)"
    ]
        |> String.Extra.spaceJoin
        |> List.singleton


generateFileContent : ModuleName -> String -> String -> GenerationRequirements -> String
generateFileContent moduleName imports content { moduleNamespace, imported } =
    [ moduleGeneration (String.join "." moduleName) moduleNamespace
    , String.Extra.spaceJoin [ "import", imported ]
    , String.Extra.spaceJoin [ "import", String.join "." moduleName, "exposing (..)" ]
    , String.Extra.spaceJoin [ "import", Generator.Static.moduleName, "exposing (..)" ]
    , imports
    , content
    ]
        |> String.Extra.newlineJoin


moduleGeneration : String -> String -> String
moduleGeneration name moduleNamespace =
    [ "module"
    , String.join "." [ name, moduleNamespace ]
    , "exposing (..)"
    ]
        |> String.Extra.spaceJoin
