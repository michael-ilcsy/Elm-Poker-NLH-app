module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, src)
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
    | InitializeGame ( Board, PlayerHand, PlayerHand )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitDeck deck ->
            model |> update (InitializeGame (initializeHoldemGame deck))

        InitializeGame ( board, playerHand1, playerHand2 ) ->
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
        , h1 [] [ text "どっちの勝ち？" ]
        , div []
            [ div [ class "player-hand" ]
                (List.append
                    (model.player1 |> Holdem.mapPlayerHand showCardImage)
                    [ playerButton ]
                )
            , div [ class "chop" ] [ chopButton ]
            , div [ class "player-hand" ]
                (List.append
                    (model.player2 |> Holdem.mapPlayerHand showCardImage)
                    [ playerButton ]
                )
            ]
        ]


{-| トランプのカードの画像を表示します
-}
showCardImage : Card -> Html msg
showCardImage card =
    img [ src (card |> toImagePath) ] []


playerButton : Html msg
playerButton =
    button
        [ class "button"
        , class "is-link"
        , class "player-button"
        ]
        [ text "勝ち" ]


chopButton : Html msg
chopButton =
    button
        [ class "button"
        , class "is-primary"
        ]
        [ text "引き分け" ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
