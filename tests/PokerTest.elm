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
            , describe "ストレートのテスト"
                [ test "ストレートのときにTrueが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Three Four Six Seven Five
                            |> isStraight
                            |> Expect.true "ストレートの手札が期待されます"
                , test "T,J,Q,K,AのストレートのときにTrueが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Ace Queen Jack King Ten
                            |> isStraight
                            |> Expect.true "T,J,Q,K,Aのストレートの手札が期待されます"
                , test "A,2,3,4,5のストレートのときにTrueが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Three Five Ace Four Two
                            |> isStraight
                            |> Expect.true "A,2,3,4,5のストレートの手札が期待されます"
                , test "ストレートじゃないときにFalseが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Ten Nine Eight Five Seven
                            |> isStraight
                            |> Expect.false "ストレートじゃない手札が期待されます"
                ]
            ]
        ]


{-| テスト用にHandを作成します
-}
makeHand : ( Suit, Rank ) -> ( Suit, Rank ) -> ( Suit, Rank ) -> ( Suit, Rank ) -> ( Suit, Rank ) -> Hand
makeHand ( s1, r1 ) ( s2, r2 ) ( s3, r3 ) ( s4, r4 ) ( s5, r5 ) =
    Hand (Card s1 r1) (Card s2 r2) (Card s3 r3) (Card s4 r4) (Card s5 r5)


{-| テスト用にスーツを気にしないHandを作成します
-}
makeHandWithoutSuit : Rank -> Rank -> Rank -> Rank -> Rank -> Hand
makeHandWithoutSuit r1 r2 r3 r4 r5 =
    Hand (Card Spade r1) (Card Hart r2) (Card Club r3) (Card Diamond r4) (Card Spade r5)
