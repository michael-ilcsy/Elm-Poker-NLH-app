module Models.Rank exposing (..)


type Rank
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


toString : Rank -> String
toString rank =
    case rank of
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


{-| Rankを数値に変換します
-}
toNumber : Rank -> Int
toNumber rank =
    case rank of
        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            11

        Queen ->
            12

        King ->
            13

        Ace ->
            14
