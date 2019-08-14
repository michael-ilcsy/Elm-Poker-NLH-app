module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "%PUBLIC_URL%/img/s01.png" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



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


type Card
    = Card Suit Rank


{-| 新品のデッキを作ります
-}
makeNewDeck =
    suits |> List.concatMap (\suit -> ranks |> List.map (Card suit))


{-| Cardから画像のパスを生成します
-}
toImagePath : Card -> String
toImagePath card =
    case card of
        Card suit rank ->
            "%PUBLIC_URL%/img/" ++ (suit |> suitToString) ++ (rank |> rankToString) ++ ".png"


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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
