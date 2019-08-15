module PokerTest exposing (all)

import Expect
import Main exposing (..)
import Test exposing (..)


all : Test
all =
    describe "pokerのテスト"
        [ describe "役判定のテスト"
            [ describe "フラッシュのテスト"
                [ test "フラッシュのときにTrueが返ること" <|
                    \_ ->
                        makeHand ( Spade, Ace ) ( Spade, Two ) ( Spade, King ) ( Spade, Queen ) ( Spade, Five )
                            |> isFlush
                            |> Expect.true "フラッシュの手札が期待されます"
                , test "フラッシュじゃないときにFalseが返ること" <|
                    \_ ->
                        makeHand ( Spade, Ace ) ( Diamond, Two ) ( Spade, King ) ( Spade, Queen ) ( Spade, Five )
                            |> isFlush
                            |> Expect.false "フラッシュじゃない手札が期待されます"
                ]
            ]
        ]


{-| テスト用にHandを作成します
-}
makeHand : ( Suit, Rank ) -> ( Suit, Rank ) -> ( Suit, Rank ) -> ( Suit, Rank ) -> ( Suit, Rank ) -> Hand
makeHand ( s1, r1 ) ( s2, r2 ) ( s3, r3 ) ( s4, r4 ) ( s5, r5 ) =
    Hand (Card s1 r1) (Card s2 r2) (Card s3 r3) (Card s4 r4) (Card s5 r5)
