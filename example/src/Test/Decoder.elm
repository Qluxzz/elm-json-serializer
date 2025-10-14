module Test.Decoder exposing (..)

import Json.Decode
import Serializer.Json.Extra
import Test exposing (..)


decodeTest : Json.Decode.Decoder Test
decodeTest =
    Json.Decode.succeed Test
        |> Serializer.Json.Extra.andMap (Json.Decode.field "foo" Json.Decode.int)
        |> Serializer.Json.Extra.andMap (Json.Decode.field "bar" Json.Decode.string)
        |> Serializer.Json.Extra.andMap (Json.Decode.field "biz" (Json.Decode.dict decodeFoo))


decodeFoo : Json.Decode.Decoder Foo
decodeFoo =
    Json.Decode.succeed Foo
        |> Serializer.Json.Extra.andMap (Json.Decode.field "a" Json.Decode.int)
        |> Serializer.Json.Extra.andMap (Json.Decode.field "b" (Json.Decode.list Json.Decode.string))
        |> Serializer.Json.Extra.andMap (Json.Decode.field "c" Json.Decode.bool)
