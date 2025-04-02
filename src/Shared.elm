module Shared exposing (..)


type alias FileName =
    String


type alias Encoder =
    String


type alias Decoder =
    String


type alias FileContent =
    String


type alias TypeName =
    String


type alias ModuleName =
    String


indexToFieldName : Int -> String
indexToFieldName index =
    case index of
        0 ->
            "first"

        1 ->
            "second"

        2 ->
            "third"

        3 ->
            "fourth"

        4 ->
            "fifth"

        5 ->
            "sixth"

        6 ->
            "seventh"

        7 ->
            "eighth"

        8 ->
            "ninth"

        10 ->
            "tenth"

        _ ->
            "WOW! You should reduce your number of arguments, or make a PR! 😄"
