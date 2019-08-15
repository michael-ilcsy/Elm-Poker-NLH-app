module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Random exposing (Generator)
import Random.List



---- MODEL ----


type alias Model =
    { deck : List Card
    }


init : ( Model, Cmd Msg )
init =
    ( { deck = [] }, makeNewShuffledDeck )



---- UPDATE ----


type Msg
    = InitDeck (List Card)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitDeck deck ->
            ( { model | deck = deck }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [] (model.deck |> List.map showCardImage)


{-| トランプのカードの画像を表示します
-}
showCardImage : Card -> Html msg
showCardImage card =
    img [ src (card |> toImagePath) ] []



---- TRUMP ----


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
makeNewShuffledDeck : Cmd Msg
makeNewShuffledDeck =
    Random.generate InitDeck <| shuffleDeck makeNewDeck


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



---- POKER ----


type Hand
    = Hand Card Card Card Card Card


type HandRank
    = RoyalStraightFlush
    | StraightFlush Int
    | FourOfaKind Int Int
    | FullHouse Int Int
    | Flush Int Int Int Int Int
    | Straight Int
    | ThreeOfaKind Int Int Int
    | TwoPair Int Int Int
    | OnePair Int Int Int Int
    | HighCard Int Int Int Int Int


{-| フラッシュかどうか調べて、HandRankを返します
-}
flush : Hand -> Maybe HandRank
flush hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                cardList =
                    [ card1, card2, card3, card4, card5 ]
            in
            if cardList |> isFlush then
                case
                    cardList
                        |> List.map toRankNumber
                        |> List.sort
                        |> List.reverse
                of
                    [ a, b, c, d, e ] ->
                        Just (Flush a b c d e)

                    _ ->
                        Nothing

            else
                Nothing


{-| フラッシュかどうか調べます
-}
isFlush : List Card -> Bool
isFlush cardList =
    case cardList of
        [ c1, c2, c3, c4, c5 ] ->
            cardList |> List.all (\card -> card.suit == c1.suit)

        _ ->
            False


{-| ストレートかどうか調べて、HandRankを返します
-}
straight : Hand -> Maybe HandRank
straight hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                cardRanks =
                    [ card1, card2, card3, card4, card5 ]
                        |> List.map toRankNumber
                        |> List.sort
                        |> List.reverse
            in
            if cardRanks == [ 14, 5, 4, 3, 2 ] then
                Just (Straight 5)

            else if cardRanks |> isStraight then
                case cardRanks of
                    [ a, b, c, d, e ] ->
                        Just (Straight a)

                    _ ->
                        Nothing

            else
                Nothing


{-| ストレートかどうか調べます
-}
isStraight : List Int -> Bool
isStraight list =
    if List.length list /= 5 then
        False

    else
        case list of
            first :: rest ->
                isStraightHelp first rest

            _ ->
                False


isStraightHelp : Int -> List Int -> Bool
isStraightHelp prev list =
    case list of
        next :: rest ->
            if prev - next == 1 then
                isStraightHelp next rest

            else
                False

        [] ->
            True


{-| ロイヤルストレートフラッシュかどうか調べて、HandRankを返します
-}
royalStraightFlush : Hand -> Maybe HandRank
royalStraightFlush hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                cardList =
                    [ card1, card2, card3, card4, card5 ]
            in
            if cardList |> isFlush then
                let
                    cardRanks =
                        cardList
                            |> List.map toRankNumber
                            |> List.sort
                            |> List.reverse
                in
                if cardRanks == [ 14, 13, 12, 11, 10 ] then
                    Just RoyalStraightFlush

                else
                    Nothing

            else
                Nothing


{-| ストレートフラッシュかどうか調べて、HandRankを返します
ロイヤルストレートフラッシュであったとしてもストレートフラッシュになります
-}
straightFlush : Hand -> Maybe HandRank
straightFlush hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                cardList =
                    [ card1, card2, card3, card4, card5 ]
            in
            if cardList |> isFlush then
                let
                    cardRanks =
                        cardList
                            |> List.map toRankNumber
                            |> List.sort
                            |> List.reverse
                in
                if cardRanks |> isStraight then
                    case cardRanks of
                        [ a, b, c, d, e ] ->
                            Just (StraightFlush a)

                        _ ->
                            Nothing

                else
                    Nothing

            else
                Nothing



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
