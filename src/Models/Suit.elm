module Models.Suit exposing (..)


type Suit
    = Spade
    | Hart
    | Club
    | Diamond


toString : Suit -> String
toString suit =
    case suit of
        Spade ->
            "s"

        Hart ->
            "h"

        Club ->
            "c"

        Diamond ->
            "d"
