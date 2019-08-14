module Trump exposing (Card, Number(..), Suit(..), toImagePath)


type Suit
    = Spade
    | Hart
    | Diamond
    | Club


type Number
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


type alias Card =
    { suit : Suit
    , number : Number
    }


toImagePath : Card -> String
toImagePath card =
    "%PUBLIC_URL%/img/" ++ (card.suit |> suitToString) ++ (card.number |> numberToString) ++ ".png"


suitToString : Suit -> String
suitToString suit =
    case suit of
        Spade ->
            "s"

        Hart ->
            "h"

        Club ->
            "c"

        Diamond ->
            "d"


numberToString : Number -> String
numberToString number =
    case number of
        Two ->
            "02"

        Three ->
            "03"

        Four ->
            "04"

        Five ->
            "05"

        Six ->
            "06"

        Seven ->
            "07"

        Eight ->
            "08"

        Nine ->
            "09"

        Ten ->
            "10"

        Jack ->
            "11"

        Queen ->
            "12"

        King ->
            "13"

        Ace ->
            "01"
