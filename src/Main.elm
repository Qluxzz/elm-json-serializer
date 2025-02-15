port module Main exposing (..)

import Declaration
import Dependency exposing (DecodersEncodersDeps, Dependency(..))
import Dict exposing (Dict)
import Elm.Parser
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Pattern exposing (moduleNames)
import Generator
import Generator.Module
import Generator.Static
import Shared
import String.Extra


type alias Model =
    { typesToGenerate : List ( Dependency, String )
    , rawFiles : Dict ModuleName RawFile
    , filesContent : Dict ModuleName DecodersEncoders
    , generatedTypes : List ( Dependency, String )
    }


addTypeNameToGenerate : ( Dependency, String ) -> Model -> Model
addTypeNameToGenerate typeName ({ typesToGenerate } as model) =
    { model | typesToGenerate = typeName :: typesToGenerate }


addTypesNameToGenerate : List ( Dependency, String ) -> Model -> Model
addTypesNameToGenerate dependencies model =
    List.foldr addTypeNameToGenerate model dependencies


addRawFile : ModuleName -> RawFile -> Model -> Model
addRawFile moduleName rawFile ({ rawFiles } as model) =
    { model | rawFiles = Dict.insert moduleName rawFile rawFiles }


type Msg
    = FileContentRead ( Shared.FileContent, Shared.TypeName )
    | MultipleFilesContentRead (List ( Shared.FileContent, ModuleName ))
    | GenerateDecodersEncoders
    | SendErrorMessage String


port fileContentRead : (( Shared.FileContent, Shared.TypeName ) -> msg) -> Sub msg


port takeThoseFiles : (List ( Shared.FileContent, ModuleName ) -> msg) -> Sub msg


port writeFile : ( Shared.Decoder, Shared.Encoder, Shared.FileName ) -> Cmd msg


port writeStaticFile : ( Shared.FileName, String ) -> Cmd msg


port killMePleaseKillMe : Bool -> Cmd msg


port theresAnErrorDude : String -> Cmd msg


port readThoseFiles : List ModuleName -> Cmd msg


type alias DecodersEncoders =
    { decoders : List String
    , encoders : List String
    , dependencies : List ( Dependency, String )
    }


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] Dict.empty Dict.empty [], Cmd.none )


updateAndThen : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateAndThen msg ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            update msg model
    in
    ( newModel, Cmd.batch [ newCmd, cmd ] )


sendErrorMessage : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
sendErrorMessage error =
    updateAndThen (SendErrorMessage error)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { typesToGenerate, filesContent, generatedTypes } =
            model
    in
    case msg of
        SendErrorMessage error ->
            ( model, theresAnErrorDude error )

        GenerateDecodersEncoders ->
            case List.head typesToGenerate of
                Nothing ->
                    ( model, writeGeneratedFiles model filesContent )

                Just ( dependency, typeName ) ->
                    if List.member ( dependency, typeName ) generatedTypes then
                        updateAndThen GenerateDecodersEncoders
                            ( removeFirstTypeToGenerate model, Cmd.none )

                    else
                        generateDecodersAndEncoders dependency typeName model

        FileContentRead ( value, name ) ->
            updateAndThen GenerateDecodersEncoders <|
                parseFileAndStoreContent value name model

        MultipleFilesContentRead dependencies ->
            dependencies
                |> List.foldr addReadFilesToRawFiles ( model, Cmd.none )
                |> updateAndThen GenerateDecodersEncoders


addReadFilesToRawFiles :
    ( Shared.FileContent, ModuleName )
    -> ( Model, Cmd Msg )
    -> ( Model, Cmd Msg )
addReadFilesToRawFiles ( content, moduleName ) ( model_, cmds ) =
    let
        parsedFile =
            Elm.Parser.parse content
    in
    case parsedFile of
        Err _ ->
            sendErrorMessage "Weird" ( model_, cmds )

        Ok rawFile ->
            ( addRawFile moduleName rawFile model_, cmds )


writeGeneratedFiles : Model -> Dict ModuleName DecodersEncoders -> Cmd Msg
writeGeneratedFiles model filesContent =
    if Dict.isEmpty filesContent then
        Cmd.none

    else
        filesContent
            |> Dict.toList
            |> List.map (writeFileContent model)
            |> List.append
                [ writeStaticFile
                    ( Generator.Static.moduleName
                    , Generator.Static.jsonDecodeExtra
                    )
                ]
            |> Cmd.batch


writeFileContent : Model -> ( ModuleName, DecodersEncoders ) -> Cmd Msg
writeFileContent { rawFiles } ( moduleName, { decoders, encoders, dependencies } ) =
    writeFile
        ( decoders
            |> String.Extra.newlineJoin
            |> Generator.Module.addModuleName rawFiles dependencies moduleName Generator.Module.Decoder
        , encoders
            |> String.Extra.newlineJoin
            |> Generator.Module.addModuleName rawFiles dependencies moduleName Generator.Module.Encoder
        , String.join "." moduleName
        )


parseFileAndStoreContent : String -> String -> Model -> ( Model, Cmd Msg )
parseFileAndStoreContent value name model =
    let
        parsedFile =
            Elm.Parser.parse value
    in
    case parsedFile of
        Err _ ->
            ( model, Cmd.none )

        Ok rawFile ->
            let
                moduleName =
                    Elm.RawFile.moduleName rawFile
            in
            case moduleName of
                [] ->
                    ( model, killMePleaseKillMe True )

                moduleName_ ->
                    ( model
                        |> addTypeNameToGenerate ( InModule moduleName_, name )
                        |> addRawFile moduleName_ rawFile
                    , Cmd.none
                    )


generateDecodersAndEncoders : Dependency -> Shared.TypeName -> Model -> ( Model, Cmd Msg )
generateDecodersAndEncoders dependency typeName model =
    let
        { rawFiles } =
            model
    in
    case dependency of
        InModule moduleName ->
            case Dict.get moduleName rawFiles of
                Nothing ->
                    ( model, readThoseFiles [ moduleName ] )

                Just rawFile ->
                    let
                        modelWithTypeDecodersAndEncoders =
                            model
                                |> addGeneratedTypes ( dependency, typeName )
                                |> storeDecodersEncodersAndDepsIn moduleName
                    in
                    updateAndThen GenerateDecodersEncoders <|
                        ( rawFile
                            |> Declaration.getDeclarationByName typeName
                            |> Maybe.andThen Generator.generateDecodersEncodersAndDeps
                            |> Maybe.map (Dependency.fetchDependencies moduleName rawFile)
                            |> Maybe.map modelWithTypeDecodersAndEncoders
                            |> Maybe.withDefault (removeFirstTypeToGenerate model)
                        , Cmd.none
                        )

        InOneOf moduleNames ->
            let
                dependencies =
                    List.map (fetchRawFile rawFiles) moduleNames
            in
            if List.member Nothing (List.map Tuple.second dependencies) then
                ( model
                , dependencies
                    |> List.concatMap removeReadFiles
                    |> readThoseFiles
                )

            else
                updateAndThen GenerateDecodersEncoders <|
                    ( model
                        |> removeFirstTypeToGenerate
                        |> addGeneratedTypes ( dependency, typeName )
                        |> addTypesNameToGenerate
                            (List.map (changeRawFilesAsDependency typeName) dependencies)
                    , Cmd.none
                    )


fetchRawFile : Dict ModuleName RawFile -> ModuleName -> ( ModuleName, Maybe RawFile )
fetchRawFile rawFiles name =
    ( name, Dict.get name rawFiles )


changeRawFilesAsDependency : String -> ( ModuleName, Maybe RawFile ) -> ( Dependency, String )
changeRawFilesAsDependency typeName =
    Tuple.mapSecond (always typeName) >> Tuple.mapFirst InModule


addGeneratedTypes : ( Dependency, String ) -> Model -> Model
addGeneratedTypes value ({ generatedTypes } as model) =
    { model | generatedTypes = value :: generatedTypes }


removeFirstTypeToGenerate : Model -> Model
removeFirstTypeToGenerate ({ typesToGenerate } as model) =
    { model | typesToGenerate = Maybe.withDefault [] (List.tail typesToGenerate) }


removeReadFiles : ( ModuleName, Maybe RawFile ) -> List ModuleName
removeReadFiles ( moduleName, rawFile ) =
    case rawFile of
        Nothing ->
            [ moduleName ]

        Just _ ->
            []


storeDecodersEncodersAndDepsIn : ModuleName -> Model -> DecodersEncodersDeps -> Model
storeDecodersEncodersAndDepsIn moduleName model { decoder, encoder, dependencies } =
    let
        { typesToGenerate, filesContent, generatedTypes } =
            model

        fileContent =
            filesContent
                |> Dict.get moduleName
                |> Maybe.withDefault
                    { encoders = []
                    , decoders = []
                    , dependencies = []
                    }
    in
    { model
        | filesContent =
            Dict.insert moduleName
                { decoders = List.append fileContent.decoders [ decoder ]
                , encoders = List.append fileContent.encoders [ encoder ]
                , dependencies = List.append fileContent.dependencies dependencies
                }
                filesContent
        , typesToGenerate =
            typesToGenerate
                |> List.tail
                |> Maybe.withDefault []
                |> List.append
                    (List.filter
                        (\elem -> not (List.member elem generatedTypes))
                        dependencies
                    )
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fileContentRead FileContentRead
        , takeThoseFiles MultipleFilesContentRead
        ]
