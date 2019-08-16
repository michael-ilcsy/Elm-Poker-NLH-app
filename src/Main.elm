module Main exposing (..)

import Browser
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src)
import Models.Card exposing (..)
import Models.Deck as Deck exposing (..)
import Poker.Holdem as Holdem exposing (..)



---- MODEL ----


type alias Model =
    { board : Board
    , player1 : PlayerHand
    , player2 : PlayerHand
    }


init : ( Model, Cmd Msg )
init =
    ( { board = dummyBoard
      , player1 = dummyPlayerHand
      , player2 = dummyPlayerHand
      }
    , makeNewShuffledDeck InitDeck
    )



---- UPDATE ----


type Msg
    = InitDeck Deck


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitDeck deck ->
            let
                ( board, playerHand1, playerHand2 ) =
                    deck |> Holdem.deal
            in
            ( { model
                | board = board
                , player1 = playerHand1
                , player2 = playerHand2
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [] (model.board |> Holdem.mapBoard showCardImage)
        , div [] (model.player1 |> Holdem.mapPlayerHand showCardImage)
        , div [] (model.player2 |> Holdem.mapPlayerHand showCardImage)
        ]


{-| トランプのカードの画像を表示します
-}
showCardImage : Card -> Html msg
showCardImage card =
    img [ src (card |> toImagePath) ] []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
