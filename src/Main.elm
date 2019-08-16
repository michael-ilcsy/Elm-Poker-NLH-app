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
    , playerChoice : PlayerChoice
    , answer : Answer
    }


type GameStatus
    = BeforeInitialize
    | Waiting
    | Result


type PlayerChoice
    = One
    | Two
    | Draw
    | NoSelect


type Answer
    = Correct
    | Wrong
    | NoAnswer


init : ( Model, Cmd Msg )
init =
    ( { board = dummyBoard
      , player1 = dummyPlayerHand
      , player1HandRankString = ""
      , player2 = dummyPlayerHand
      , player2HandRankString = ""
      , gameStatus = BeforeInitialize
      , gameResult = Chop
      , playerChoice = NoSelect
      , answer = NoAnswer
      }
    , makeNewShuffledDeck InitDeck
    )



---- UPDATE ----


type Msg
    = InitDeck Deck
    | InitializeGame ( Board, PlayerHand, PlayerHand )
    | SelectPlayer PlayerChoice
    | RestartGame


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

        SelectPlayer select ->
            let
                ( result, p1string, p2string ) =
                    Holdem.judgeHandRank model.board model.player1 model.player2

                answer =
                    if select == One then
                        if result == Win then
                            Correct

                        else
                            Wrong

                    else if select == Two then
                        if result == Lose then
                            Correct

                        else
                            Wrong

                    else if result == Chop then
                        Correct

                    else
                        Wrong
            in
            ( { model
                | player1HandRankString = p1string
                , player2HandRankString = p2string
                , gameStatus = Result
                , gameResult = result
                , playerChoice = select
                , answer = answer
              }
            , Cmd.none
            )

        RestartGame ->
            ( model, makeNewShuffledDeck InitDeck )



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
                        (if model.gameStatus == Result then
                            []

                         else
                            [ playerButton (onClick (SelectPlayer One)) ]
                        )
                        |> resultView model model.player1HandRankString Win
                    )
                , if model.gameStatus == Result then
                    div
                        [ class "chop"
                        , if model.gameResult == Chop then
                            class "winner"

                          else
                            class ""
                        ]
                        [ if model.gameResult == Chop then
                            text "引き分け"

                          else
                            text ""
                        , if model.playerChoice == Draw then
                            div [ class "arrow" ] []

                          else
                            div [] []
                        ]

                  else
                    div [ class "chop" ] [ chopButton (onClick (SelectPlayer Draw)) ]
                , div [ class "player-hand" ]
                    (List.append
                        (model.player2 |> Holdem.mapPlayerHand showCardImage)
                        (if model.gameStatus == Result then
                            []

                         else
                            [ playerButton (onClick (SelectPlayer Two)) ]
                        )
                        |> resultView model model.player2HandRankString Lose
                    )
                ]
            , if model.gameStatus == Result then
                if model.answer == Correct then
                    div [ class "has-text-link", class "result" ]
                        [ text "正解！"
                        , button
                            [ class "button"
                            , class "is-info"
                            , class "result-button"
                            , onClick RestartGame
                            ]
                            [ text "もう一度" ]
                        ]

                else
                    div [ class "has-text-danger", class "result" ]
                        [ text "不正解！"
                        , button
                            [ class "button"
                            , class "is-info"
                            , class "result-button"
                            , onClick RestartGame
                            ]
                            [ text "もう一度" ]
                        ]

              else
                div [] []
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


chopButton : Attribute msg -> Html msg
chopButton attr =
    button
        [ class "button"
        , class "is-primary"
        , attr
        ]
        [ text "引き分け" ]


resultView : Model -> String -> Holdem.Result -> List (Html msg) -> List (Html msg)
resultView model handRankString result viewList =
    List.append
        viewList
        (if model.gameStatus == Result then
            [ div
                [ if model.gameResult == result then
                    class "winner"

                  else
                    class ""
                ]
                [ text handRankString
                , if model.playerChoice == One then
                    if result == Win then
                        div [ class "arrow" ] []

                    else
                        div [] []

                  else if model.playerChoice == Two then
                    if result == Lose then
                        div [ class "arrow" ] []

                    else
                        div [] []

                  else
                    div [] []
                ]
            ]

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
