module Generator.Static exposing (..)

import String.Extra


moduleName : String
moduleName =
    "Serializer.Json.Extra"


jsonDecodeExtra : String
jsonDecodeExtra =
    [ "module " ++ moduleName ++ " exposing (..)"
    , "import Json.Decode"
    , "import Json.Encode"
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
    [ "andMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b"
    , "andMap = Json.Decode.map2 (|>)"
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
    [ "encodeMaybe : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value"
    , "encodeMaybe mapper field ="
    , "  Maybe.withDefault Json.Encode.null (Maybe.map mapper field)"
    ]
        |> String.Extra.newlineJoin
