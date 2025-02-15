module Generator.Encoder exposing (..)

import Elm.Syntax.Node as Node
import Elm.Syntax.Range as Range
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAlias as Alias
import Elm.Syntax.TypeAnnotation as Annotation
import Shared
import String.Extra


aliasDeclEncoder : String -> Alias.TypeAlias -> String
aliasDeclEncoder recordName { name, typeAnnotation } =
    [ "Encode.object"
    , typeAnnotation
        |> Node.value
        |> typeAnnotationEncoder recordName ""
        |> String.Extra.surroundByBrackets
    ]
        |> String.Extra.spaceJoin
        |> String.Extra.surroundByParen


generateAliasEncoder : Alias.TypeAlias -> String
generateAliasEncoder ({ name } as declaration) =
    let
        functionName =
            "encode" ++ Node.value name
    in
    [ [ functionName, ":", Node.value name, "-> Encode.Value" ]
    , [ functionName, "record =" ]
    , [ String.Extra.indent (aliasDeclEncoder "record" declaration) ]
    ]
        |> List.map String.Extra.spaceJoin
        |> String.Extra.newlineJoin


generateTypedEncoder : Type.Type -> String
generateTypedEncoder ({ name } as type_) =
    let
        functionName =
            "encode" ++ Node.value name
    in
    [ [ functionName, ":", Node.value name, "-> Encode.Value" ]
    , [ functionName, "record =" ]
    , [ String.Extra.indent (generateTypedEncoderHelp type_) ]
    ]
        |> List.map String.Extra.spaceJoin
        |> String.Extra.newlineJoin


generateTypedEncoderHelp : Type.Type -> String
generateTypedEncoderHelp { name, generics, constructors } =
    [ [ "case record of" ]
    , constructors
        |> List.map Node.value
        |> List.map constructorEncoder
        |> List.map String.Extra.indent
        |> List.map String.Extra.indent
    ]
        |> List.map String.Extra.newlineJoin
        |> String.Extra.newlineJoin


caseConstructor : Type.ValueConstructor -> String
caseConstructor { name, arguments } =
    let
        fieldNameForArguments : Int -> b -> String
        fieldNameForArguments index _ =
            Shared.indexToFieldName index
    in
    [ Node.value name
    , arguments
        |> List.indexedMap fieldNameForArguments
        |> String.Extra.spaceJoin
    , "->"
    ]
        |> String.Extra.spaceJoin


constructorBodyEncoder : Type.ValueConstructor -> String
constructorBodyEncoder { name, arguments } =
    [ [ String.Extra.surroundByQuotes "type"
      , ","
      , "Encode.string"
      , String.Extra.surroundByQuotes (Node.value name)
      ]
        |> String.Extra.spaceJoin
        |> String.Extra.surroundByParen
        |> List.singleton
    , arguments
        |> List.indexedMap argumentEncoder
    ]
        |> List.concat
        |> String.join "\n    ,"
        |> String.Extra.surroundByBrackets


constructorEncoder : Type.ValueConstructor -> String
constructorEncoder ({ name, arguments } as type_) =
    [ "Encode.object"
    , constructorBodyEncoder type_
    ]
        |> List.map String.Extra.indent
        |> String.join "\n  "
        |> String.append (caseConstructor type_)


argumentEncoder : Int -> Node.Node Annotation.TypeAnnotation -> String
argumentEncoder index annotation =
    let
        fieldName =
            Shared.indexToFieldName index
    in
    [ String.Extra.surroundByQuotes fieldName
    , encloseArgumentBody fieldName (Node.value annotation)
    ]
        |> String.join ", "
        |> String.Extra.surroundByParen


encloseArgumentBody : String -> Annotation.TypeAnnotation -> String
encloseArgumentBody fieldName annotation =
    let
        annotationEncoderBody =
            typeAnnotationEncoder fieldName "" annotation
    in
    String.Extra.spaceJoin <|
        case annotation of
            Annotation.Record definition ->
                [ "Encode.object", String.Extra.surroundByBrackets annotationEncoderBody ]

            Annotation.GenericType type_ ->
                [ annotationEncoderBody, fieldName ]

            Annotation.Typed _ _ ->
                [ annotationEncoderBody, fieldName ]

            _ ->
                []


typeAnnotationEncoder : String -> String -> Annotation.TypeAnnotation -> String
typeAnnotationEncoder recordName fieldName typeAnnotation =
    case typeAnnotation of
        Annotation.Record definition ->
            recordEncoder recordName definition

        Annotation.GenericType type_ ->
            genericTypeEncoder recordName fieldName type_ []

        Annotation.Typed node annotations ->
            let
                ( _, value ) =
                    Node.value node
            in
            typedEncoder recordName fieldName value annotations

        -- Annotation.Unit ->
        Annotation.Tupled annotations ->
            tupleEncoder recordName fieldName annotations

        -- Annotation.GenericRecord name definition ->
        -- Annotation.FunctionTypeAnnotation annotation annotation ->
        _ ->
            ""


tupleEncoder : String -> String -> List (Node.Node Annotation.TypeAnnotation) -> String
tupleEncoder recordName fieldName annotations =
    annotations
        |> List.map Node.value
        |> List.map (typeAnnotationEncoder recordName fieldName)
        |> List.indexedMap (tupleFieldEncoder recordName fieldName)
        |> String.join "\n, "
        |> String.Extra.surroundByBrackets
        |> String.append "Encode.object"


tupleFieldEncoder : String -> String -> Int -> String -> String
tupleFieldEncoder recordName fieldName index value =
    String.Extra.surroundByParen
        (String.Extra.surroundByQuotes (String.fromInt index)
            ++ ", "
            ++ value
            ++ String.Extra.surroundByParen
                ("Tuple."
                    ++ (if index == 0 then
                            "first"

                        else
                            "second"
                       )
                    ++ " "
                    ++ recordName
                    ++ "."
                    ++ fieldName
                )
        )


genericTypeEncoder : String -> String -> String -> List (Node.Node Annotation.TypeAnnotation) -> String
genericTypeEncoder recordName fieldName type_ annotations =
    case type_ of
        "String" ->
            "Encode.string"

        "Int" ->
            "Encode.int"

        "Float" ->
            "Encode.float"

        "Bool" ->
            "Encode.bool"

        "List" ->
            annotationInGenericTypeEncoder recordName fieldName "Encode.list" annotations

        "Maybe" ->
            annotationInGenericTypeEncoder recordName fieldName "encodeMaybe" annotations

        "Array" ->
            annotationInGenericTypeEncoder recordName fieldName "Encode.array" annotations

        "Set" ->
            annotationInGenericTypeEncoder recordName fieldName "Encode.set" annotations

        "Dict" ->
            let
                -- Key type must be `comparable`
                --  This includes numbers, characters, strings, lists of comparable things, and tuples of comparable things.
                keyTypeToString =
                    List.head annotations
                        |> Maybe.map
                            (\kT ->
                                case Node.value kT of
                                    Annotation.Typed (Node.Node _ ( _, keyValueType )) [] ->
                                        case keyValueType of
                                            "String" ->
                                                "identity"

                                            "Int" ->
                                                "String.fromInt"

                                            _ ->
                                                "Unhandled type"

                                    _ ->
                                        "Invalid type"
                            )
                        |> Maybe.withDefault "Invalid key type"
            in
            [ "Encode.dict"
            , keyTypeToString
            , annotationInGenericTypeEncoder recordName fieldName "" (List.drop 1 annotations)
            ]
                |> String.Extra.spaceJoin

        value ->
            "encode" ++ value


annotationInGenericTypeEncoder : String -> String -> String -> List (Node.Node Annotation.TypeAnnotation) -> String
annotationInGenericTypeEncoder recordName fieldName encoder annotations =
    annotations
        |> List.map Node.value
        |> List.map (typeAnnotationEncoder recordName fieldName)
        |> String.Extra.spaceJoin
        |> String.Extra.surroundByParen
        |> String.append encoder


typedEncoder : String -> String -> String -> List (Node.Node Annotation.TypeAnnotation) -> String
typedEncoder recordName fieldName type_ annotations =
    genericTypeEncoder recordName fieldName type_ annotations


recordEncoder : String -> Annotation.RecordDefinition -> String
recordEncoder recordName definition =
    definition
        |> List.map Node.value
        |> List.map (recordFieldEncoder recordName)
        |> String.join "\n, "


isTuple : Annotation.TypeAnnotation -> Bool
isTuple annotation =
    case annotation of
        Annotation.Tupled _ ->
            True

        _ ->
            False


recordFieldEncoder : String -> Annotation.RecordField -> String
recordFieldEncoder recordName recordField =
    let
        name =
            Node.value (Tuple.first recordField)

        content =
            Tuple.second recordField
    in
    [ String.Extra.surroundByQuotes name
    , String.Extra.surroundByParen
        (typeAnnotationEncoder recordName name (Node.value content)
            ++ (if isTuple (Node.value content) then
                    ""

                else
                    " " ++ recordName ++ "." ++ name
               )
        )
    ]
        |> String.join ", "
        |> String.Extra.surroundByParen
