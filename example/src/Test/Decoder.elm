module Test.Decoder exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Serializer.Json.Extra exposing (..)
import Test exposing (..)


decodeTest : Decoder Test
decodeTest =
    Decode.succeed Test
        |> andMap (Decode.field "foo" Decode.int)
        |> andMap (Decode.field "bar" Decode.string)
        |> andMap (Decode.field "biz" (Decode.dict decodeFoo))


decodeFoo : Decoder Foo
decodeFoo =
    Decode.succeed Foo
        |> andMap (Decode.field "a" Decode.int)
        |> andMap (Decode.field "b" (Decode.list Decode.string))
        |> andMap (Decode.field "c" Decode.bool)
