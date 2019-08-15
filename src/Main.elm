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



---- POKER ----


type Hand
    = Hand Card Card Card Card Card


type HandRank
    = RoyalStraightFlush
    | StraightFlush Rank
    | FourOfaKind Rank Rank
    | FullHouse Rank Rank
    | Flush Rank Rank Rank Rank Rank
    | Straight Rank
    | ThreeOfaKind Rank Rank Rank
    | TwoPair Rank Rank Rank
    | OnePair Rank Rank Rank Rank
    | HighCard Rank Rank Rank Rank Rank


{-| フラッシュかどうか調べます
-}
isFlush : Hand -> Bool
isFlush hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                suit =
                    card1.suit
            in
            [ card1, card2, card3, card4, card5 ] |> List.all (\card -> card.suit == suit)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
