module Serializer.Json.Extra exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Decode.map2 (|>)


tupleThree : a -> b -> c -> ( a, b, c )
tupleThree a b c =
    ( a, b, c )


encodeMaybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeMaybe mapper field =
    Maybe.withDefault Encode.null (Maybe.map mapper field)
