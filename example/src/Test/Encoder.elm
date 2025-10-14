module Test.Encoder exposing (..)

import Json.Encode
import Serializer.Json.Extra
import Test exposing (..)


encodeTest : Test -> Json.Encode.Value
encodeTest record =
    Json.Encode.object
        [ ( "foo", Json.Encode.int record.foo )
        , ( "bar", Json.Encode.string record.bar )
        , ( "biz", Json.Encode.dict identity encodeFoo record.biz )
        ]


encodeFoo : Foo -> Json.Encode.Value
encodeFoo record =
    Json.Encode.object
        [ ( "a", Json.Encode.int record.a )
        , ( "b", Json.Encode.list Json.Encode.string record.b )
        , ( "c", Json.Encode.bool record.c )
        ]
