module CardTest exposing (..)

import Expect
import Models.Card exposing (Card, toImagePath)
import Models.Rank exposing (Rank(..))
import Models.Suit exposing (Suit(..))
import Test exposing (..)


all : Test
all =
    describe "トランプのテスト"
        [ describe "カード画像のパス取得のテスト"
            [ test "ダイヤのエース" <|
                \_ ->
                    Expect.equal "%PUBLIC_URL%/img/d01.png" <| (Card Diamond Ace |> toImagePath)
            , test "スペードのキング" <|
                \_ ->
                    Expect.equal "%PUBLIC_URL%/img/s13.png" <| (Card Spade King |> toImagePath)
            ]
        ]
