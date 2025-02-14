module Generator.Decoder exposing (..)

import Dependency exposing (Dependency(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.Syntax.Range as Range
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAlias as Alias
import Elm.Syntax.TypeAnnotation as Annotation
import Shared
import String.Extra


generateAliasDecoderAndDeps : Alias.TypeAlias -> ( String, List ( Dependency, String ) )
generateAliasDecoderAndDeps declaration =
    let
        decoderAndDeps =
            aliasDeclDecoderAndDeps declaration
    in
    Tuple.mapFirst (encloseInDecoderFunction (Node.value declaration.name)) decoderAndDeps


generateTypedDecoderAndDeps : Type.Type -> ( String, List ( Dependency, String ) )
generateTypedDecoderAndDeps type_ =
    let
        decoderAndDeps =
            typedDeclDecoderAndDeps type_
    in
    Tuple.mapFirst (encloseInDecoderFunction (Node.value type_.name)) decoderAndDeps


aliasDeclDecoderAndDeps : Alias.TypeAlias -> ( String, List ( Dependency, String ) )
aliasDeclDecoderAndDeps { name, typeAnnotation } =
    typeAnnotation
        |> Node.value
        |> typeAnnotationDecoder
        |> Tuple.mapFirst (addDecoderPipelineStructure (Node.value name))


typedDeclDecoderAndDeps : Type.Type -> ( String, List ( Dependency, String ) )
typedDeclDecoderAndDeps { name, generics, constructors } =
    let
        ( body, dependencies ) =
            unionConstructor (List.map Node.value constructors)

        n =
            Node.value name
    in
    ( [ baseUnionDecoder n
      , unionConstructorName n
      , body
      ]
        |> String.Extra.newlineJoin
    , dependencies
    )


unionConstructorName : String -> String
unionConstructorName name =
    let
        functionName =
            "as" ++ name ++ "Constructor"
    in
    [ functionName ++ " : Decoder " ++ name
    , functionName ++ " value ="
    ]
        |> String.Extra.newlineJoin


unionConstructor : List Type.ValueConstructor -> ( String, List ( Dependency, String ) )
unionConstructor constructors =
    let
        constructs =
            constructors
                |> List.map constructorDecoder
                |> List.map (Tuple.mapFirst String.Extra.indent)

        constructorCases =
            constructs
                |> List.map Tuple.first
                |> String.Extra.newlineJoin

        casePlaceholder =
            [ "case value of"
            , constructorCases
            , String.Extra.indent "_ -> Decode.fail"
            ]
    in
    ( String.Extra.newlineJoin casePlaceholder
    , List.concatMap Tuple.second constructs
    )


constructorDecoder : Type.ValueConstructor -> ( String, List ( Dependency, String ) )
constructorDecoder { name, arguments } =
    let
        decoders =
            arguments
                |> List.indexedMap argumentDecoder
                |> List.map (Tuple.mapFirst String.Extra.surroundByParen)
    in
    ( [ decoderHeader (Node.value name) arguments
      , decoders
            |> List.map Tuple.first
            |> String.join "|> andMap"
            |> String.Extra.indent
            |> String.Extra.indent
      ]
        |> String.Extra.newlineJoin
    , List.concatMap Tuple.second decoders
    )


decoderHeader : String -> List a -> String
decoderHeader name arguments =
    [ String.Extra.surroundByQuotes name
    , "-> Decode.succeed"
    , name
    , if List.length arguments > 0 then
        "|> andMap"

      else
        ""
    ]
        |> String.Extra.spaceJoin


argumentDecoder :
    Int
    -> Node.Node Annotation.TypeAnnotation
    -> ( String, List ( Dependency, String ) )
argumentDecoder index typeAnnotationNode =
    typeAnnotationNode
        |> Node.value
        |> typeAnnotationDecoder
        |> Tuple.mapFirst (putDecodeIndexedField index (Node.value typeAnnotationNode))


putDecodeIndexedField : Int -> Annotation.TypeAnnotation -> String -> String
putDecodeIndexedField index annotation decoder =
    [ decodeIndexedField index
    , putRecordBaseIfNeeded annotation decoder
    ]
        |> String.Extra.spaceJoin


decodeIndexedField : Int -> String
decodeIndexedField index =
    [ "Decode.field"
    , index
        |> Shared.indexToFieldName
        |> String.Extra.surroundByQuotes
    ]
        |> String.Extra.spaceJoin


putRecordBaseIfNeeded : Annotation.TypeAnnotation -> String -> String
putRecordBaseIfNeeded annotation decoder =
    case annotation of
        Annotation.Record definition ->
            decoder
                |> String.append
                    ("Decode.succeed " ++ anonymousRecord definition ++ "\n|> andMap")
                |> String.Extra.surroundByParen

        _ ->
            decoder


anonymousRecord : Annotation.RecordDefinition -> String
anonymousRecord definition =
    let
        lambdaArgs =
            definition
                |> List.map Node.value
                |> List.map Tuple.first
                |> List.map Node.value
                |> String.Extra.spaceJoin

        lambdaBodyArg name =
            name ++ " = " ++ name

        lambdaBody =
            definition
                |> List.map Node.value
                |> List.map Tuple.first
                |> List.map Node.value
                |> List.map lambdaBodyArg
                |> String.join ", "
    in
    [ "\\", lambdaArgs, "->", "{", lambdaBody, "}" ]
        |> String.join ""
        |> String.Extra.surroundByParen


typeAnnotationDecoder : Annotation.TypeAnnotation -> ( String, List ( Dependency, String ) )
typeAnnotationDecoder typeAnnotation =
    case typeAnnotation of
        Annotation.Record definition ->
            recordDecoder definition

        Annotation.GenericType type_ ->
            genericTypeDecoder type_ []

        Annotation.Typed node annotations ->
            let
                ( moduleName, value ) =
                    Node.value node
            in
            typedDecoder moduleName value annotations

        -- Annotation.Unit ->
        Annotation.Tupled annotations ->
            tupledDecoder annotations

        -- Annotation.GenericRecord name definition ->
        -- Annotation.FunctionTypeAnnotation annotation annotation ->
        _ ->
            ( "", [] )


recordDecoder : Annotation.RecordDefinition -> ( String, List ( Dependency, String ) )
recordDecoder definition =
    definition
        |> List.map Node.value
        |> List.map recordFieldDecoder
        |> flattenTuples
        |> Tuple.mapFirst (String.join "|> andMap ")


recordFieldDecoder :
    Annotation.RecordField
    -> ( String, List ( Dependency, String ) )
recordFieldDecoder ( name, content ) =
    Node.value content
        |> typeAnnotationDecoder
        |> Tuple.mapFirst (recordFieldDecoderString (Node.value name))


recordFieldDecoderString : String -> String -> String
recordFieldDecoderString name decoder =
    [ "Decode.field"
    , String.Extra.surroundByQuotes name
    , String.Extra.surroundByParen decoder
    ]
        |> String.Extra.spaceJoin
        |> String.Extra.surroundByParen


flattenTuples :
    List ( String, List ( Dependency, String ) )
    -> ( List String, List ( Dependency, String ) )
flattenTuples =
    List.foldr concatDecoderFieldsAndKeepDeps ( [], [] )


concatDecoderFieldsAndKeepDeps :
    ( String, List ( Dependency, String ) )
    -> ( List String, List ( Dependency, String ) )
    -> ( List String, List ( Dependency, String ) )
concatDecoderFieldsAndKeepDeps ( content, deps ) ( accDecoder, accDeps ) =
    ( content :: accDecoder, accDeps ++ deps )


genericTypeDecoder : String -> List (Node.Node Annotation.TypeAnnotation) -> ( String, List ( Dependency, String ) )
genericTypeDecoder type_ annotations =
    case type_ of
        "String" ->
            ( "Decode.string", [] )

        "Int" ->
            ( "Decode.int", [] )

        "Float" ->
            ( "Decode.float", [] )

        "Bool" ->
            ( "Decode.bool", [] )

        "List" ->
            let
                ( annotations_, dependencies ) =
                    annotations
                        |> List.map Node.value
                        |> List.map typeAnnotationDecoder
                        |> flattenTuples
            in
            ( "Decode.list" ++ String.Extra.surroundByParen (String.Extra.spaceJoin annotations_), dependencies )

        "Maybe" ->
            let
                ( annotations_, dependencies ) =
                    annotations
                        |> List.map Node.value
                        |> List.map typeAnnotationDecoder
                        |> flattenTuples
            in
            ( "Decode.nullable" ++ String.Extra.surroundByParen (String.Extra.spaceJoin annotations_), dependencies )

        "Array" ->
            let
                ( annotations_, dependencies ) =
                    annotations
                        |> List.map Node.value
                        |> List.map typeAnnotationDecoder
                        |> flattenTuples
            in
            ( "Decode.array" ++ String.Extra.surroundByParen (String.Extra.spaceJoin annotations_), dependencies )

        "Set" ->
            let
                ( annotations_, dependencies ) =
                    annotations
                        |> List.map Node.value
                        |> List.map typeAnnotationDecoder
                        |> flattenTuples
            in
            ( "Decode.map (Array.toList >> Set.fromList) " ++ String.Extra.surroundByParen ("Decode.array" ++ String.Extra.surroundByParen (String.Extra.spaceJoin annotations_)), dependencies )

        value ->
            ( "decode" ++ value, [] )


typedDecoder :
    List String
    -> String
    -> List (Node.Node Annotation.TypeAnnotation)
    -> ( String, List ( Dependency, String ) )
typedDecoder moduleName type_ annotations =
    let
        ( content, deps ) =
            genericTypeDecoder type_ annotations
    in
    ( content, deps ++ dependencyIfNotGenericType moduleName type_ )


dependencyIfNotGenericType : ModuleName -> String -> List ( Dependency, String )
dependencyIfNotGenericType moduleName type_ =
    case type_ of
        "String" ->
            []

        "Int" ->
            []

        "Float" ->
            []

        "Bool" ->
            []

        "List" ->
            []

        "Maybe" ->
            []

        "Set" ->
            []

        value ->
            [ ( InModule moduleName, type_ ) ]


tupledDecoder :
    List (Node.Node Annotation.TypeAnnotation)
    -> ( String, List ( Dependency, String ) )
tupledDecoder annotations =
    annotations
        |> List.map Node.value
        |> List.map typeAnnotationDecoder
        |> flattenTuples
        |> Tuple.mapFirst (List.indexedMap tupleFieldDecoder)
        |> Tuple.mapFirst (String.join "\n ")
        |> Tuple.mapFirst (addTupleMapper annotations)


tupleFieldDecoder : Int -> String -> String
tupleFieldDecoder index value =
    [ "Decode.field"
    , index
        |> String.fromInt
        |> String.Extra.surroundByQuotes
    , value
    ]
        |> String.Extra.spaceJoin
        |> String.Extra.surroundByParen


addTupleMapper : List annotations -> String -> String
addTupleMapper annotations =
    String.append <|
        case List.length annotations of
            2 ->
                "Decode.map Tuple.pair"

            3 ->
                "Decode.map tupleThree"

            4 ->
                "Decode.map tupleFour"

            _ ->
                "We should add more cases here..."


addDecoderPipelineStructure : String -> String -> String
addDecoderPipelineStructure name decoder =
    [ [ "Decode.succeed", name, String.Extra.newline "|> andMap" ]
    , [ decoder ]
    ]
        |> List.map String.Extra.spaceJoin
        |> String.Extra.spaceJoin
        |> String.Extra.surroundByParen


encloseInDecoderFunction : String -> String -> String
encloseInDecoderFunction name decoder =
    let
        functionName =
            "decode" ++ name
    in
    [ [ functionName, ": Decoder", name ]
    , [ functionName, "=" ]
    , [ String.Extra.indent decoder ]
    ]
        |> List.map String.Extra.spaceJoin
        |> String.Extra.newlineJoin


baseUnionDecoder : String -> String
baseUnionDecoder name =
    [ "Decode.andThen" ++ " as" ++ name ++ "Constructor"
    , [ "Decode.field"
      , String.Extra.surroundByQuotes "type"
      , "Decode.string"
      ]
        |> String.Extra.spaceJoin
        |> String.Extra.surroundByParen
    ]
        |> String.join "\n "
