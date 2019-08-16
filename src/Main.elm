module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, h1, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Models.Card exposing (..)
import Models.Deck as Deck exposing (..)
import Poker.Holdem as Holdem exposing (..)



---- MODEL ----


type alias Model =
    { board : Board
    , player1 : PlayerHand
    , player1HandRankString : String
    , player2 : PlayerHand
    , player2HandRankString : String
    , gameStatus : GameStatus
    , gameResult : Holdem.Result
    }


type GameStatus
    = BeforeInitialize
    | Waiting
    | Result


init : ( Model, Cmd Msg )
init =
    ( { board = dummyBoard
      , player1 = dummyPlayerHand
      , player1HandRankString = ""
      , player2 = dummyPlayerHand
      , player2HandRankString = ""
      , gameStatus = BeforeInitialize
      , gameResult = Chop
      }
    , makeNewShuffledDeck InitDeck
    )



---- UPDATE ----


type Msg
    = InitDeck Deck
    | InitializeGame ( Board, PlayerHand, PlayerHand )
    | SelectPlayer


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
                , gameStatus = Waiting
              }
            , Cmd.none
            )

        SelectPlayer ->
            let
                ( result, p1string, p2string ) =
                    Holdem.judgeHandRank model.board model.player1 model.player2
            in
            ( { model
                | player1HandRankString = p1string
                , player2HandRankString = p2string
                , gameStatus = Result
                , gameResult = result
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    if model.gameStatus /= BeforeInitialize then
        div []
            [ div [] (model.board |> Holdem.mapBoard showCardImage)
            , h1 [] [ text "どっちの勝ち？" ]
            , div []
                [ div [ class "player-hand" ]
                    (List.append
                        (model.player1 |> Holdem.mapPlayerHand showCardImage)
                        [ playerButton (onClick SelectPlayer) ]
                        |> resultView model.gameStatus model.player1HandRankString
                    )
                , div [ class "chop" ] [ chopButton ]
                , div [ class "player-hand" ]
                    (List.append
                        (model.player2 |> Holdem.mapPlayerHand showCardImage)
                        [ playerButton (onClick SelectPlayer) ]
                        |> resultView model.gameStatus model.player2HandRankString
                    )
                ]
            ]

    else
        div [] []


{-| トランプのカードの画像を表示します
-}
showCardImage : Card -> Html msg
showCardImage card =
    img [ src (card |> toImagePath) ] []


playerButton : Attribute msg -> Html msg
playerButton attr =
    button
        [ class "button"
        , class "is-link"
        , class "player-button"
        , attr
        ]
        [ text "勝ち" ]


chopButton : Html msg
chopButton =
    button
        [ class "button"
        , class "is-primary"
        ]
        [ text "引き分け" ]


resultView : GameStatus -> String -> List (Html msg) -> List (Html msg)
resultView gameStatus handRankString viewList =
    List.append
        viewList
        (if gameStatus == Result then
            [ div [] [ text handRankString ] ]

         else
            [ div [] [] ]
        )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
