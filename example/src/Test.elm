module Test exposing (..)

import Dict exposing (Dict)
import Time


type alias Foo =
    { a : Int
    , b : List String
    , c : Bool
    , d : Time.Posix
    }


type alias Test =
    { foo : Int
    , bar : String
    , biz : Dict String Foo
    }
