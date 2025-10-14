module Serializer.Json.Extra exposing (..)

import Json.Decode
import Json.Encode


andMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
andMap =
    Json.Decode.map2 (|>)


tupleThree : a -> b -> c -> ( a, b, c )
tupleThree a b c =
    ( a, b, c )


encodeMaybe : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
encodeMaybe mapper field =
    Maybe.withDefault Json.Encode.null (Maybe.map mapper field)
