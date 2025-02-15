module Test.Encoder exposing (..)

import Json.Encode as Encode
import Serializer.Json.Extra exposing (..)
import Test exposing (..)


encodeTest : Test -> Encode.Value
encodeTest record =
    Encode.object
        [ ( "foo", Encode.int record.foo )
        , ( "bar", Encode.string record.bar )
        , ( "biz", Encode.dict identity encodeFoo record.biz )
        ]


encodeFoo : Foo -> Encode.Value
encodeFoo record =
    Encode.object
        [ ( "a", Encode.int record.a )
        , ( "b", Encode.list Encode.string record.b )
        , ( "c", Encode.bool record.c )
        ]
