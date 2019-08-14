module TrumpTest exposing (..)

import Expect
import Main exposing (..)
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
