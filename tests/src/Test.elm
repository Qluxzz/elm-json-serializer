module Test exposing (..)
import Dict exposing (Dict)

type alias Foo = 
    { a : Int
    , b : List String
    , c : Bool
    }

type alias Test =
    { foo : Int
    , bar : String
    , biz : Dict String Foo
    }
