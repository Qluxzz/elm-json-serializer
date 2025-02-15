module Generator.Static exposing (..)

import String.Extra


moduleName : String
moduleName =
    "Serializer.Json.Extra"


jsonDecodeExtra : String
jsonDecodeExtra =
    [ "module " ++ moduleName ++ " exposing (..)"
    , "import Json.Decode as Decode exposing (Decoder)"
    , "import Json.Encode as Encode"
    , "\n"
    , "\n"
    , andMapFunction
    , "\n"
    , tupleThreeFunction
    , "\n"
    , encodeMaybeFunction
    ]
        |> String.Extra.newlineJoin


andMapFunction : String
andMapFunction =
    [ "andMap : Decoder a -> Decoder (a -> b) -> Decoder b"
    , "andMap = Decode.map2 (|>)"
    ]
        |> String.Extra.newlineJoin


tupleThreeFunction : String
tupleThreeFunction =
    [ "tupleThree : a -> b -> c -> (a, b, c)"
    , "tupleThree a b c = (a, b, c)"
    ]
        |> String.Extra.newlineJoin


encodeMaybeFunction : String
encodeMaybeFunction =
    [ "encodeMaybe : (a -> Encode.Value) -> Maybe a -> Encode.Value"
    , "encodeMaybe mapper field ="
    , "  Maybe.withDefault Encode.null (Maybe.map mapper field)"
    ]
        |> String.Extra.newlineJoin
