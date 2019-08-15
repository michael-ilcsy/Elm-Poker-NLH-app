module Trump exposing (..)

import Random exposing (Generator)
import Random.List


type Suit
    = Spade
    | Hart
    | Club
    | Diamond


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


suits =
    [ Spade, Hart, Club, Diamond ]


ranks =
    [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]


type alias Card =
    { suit : Suit
    , rank : Rank
    }


{-| 新品のデッキを作ります
-}
makeNewDeck : List Card
makeNewDeck =
    suits |> List.concatMap (\suit -> ranks |> List.map (Card suit))


shuffleDeck : List Card -> Generator (List Card)
shuffleDeck deck =
    Random.List.shuffle deck


{-| シャッフルされた新品のデッキを作ります
-}
makeNewShuffledDeck : (List Card -> msg) -> Cmd msg
makeNewShuffledDeck msg =
    Random.generate msg <| shuffleDeck makeNewDeck


{-| Cardから画像のパスを生成します
-}
toImagePath : Card -> String
toImagePath card =
    "%PUBLIC_URL%/img/" ++ (card.suit |> suitToString) ++ (card.rank |> rankToString) ++ ".png"


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


rankToString : Rank -> String
rankToString rank =
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
rankToNumber : Rank -> Int
rankToNumber rank =
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


{-| CardからRankの表す数値に変換します
-}
toRankNumber : Card -> Int
toRankNumber card =
    card.rank |> rankToNumber
