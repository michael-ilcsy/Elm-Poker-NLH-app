module PokerTest exposing (all)

import Expect
import Poker exposing (..)
import Test exposing (..)
import Trump exposing (..)


all : Test
all =
    describe "pokerのテスト"
        [ describe "役判定のテスト"
            [ describe "ロイヤルストレートフラッシュのテスト"
                [ test "ロイヤルストレートフラッシュ: テストケース1" <|
                    \_ ->
                        makeHand ( Spade, Ace ) ( Spade, Jack ) ( Spade, King ) ( Spade, Queen ) ( Spade, Ten )
                            |> judgeHandRank
                            |> Expect.equal RoyalStraightFlush
                ]
            , describe
                "ストレートフラッシュのテスト"
                [ test "ストレートフラッシュ: テストケース1" <|
                    \_ ->
                        makeHand ( Spade, Nine ) ( Spade, Jack ) ( Spade, King ) ( Spade, Queen ) ( Spade, Ten )
                            |> judgeHandRank
                            |> Expect.equal (StraightFlush 13)
                , test "ストレートフラッシュ: テストケース2" <|
                    \_ ->
                        makeHand ( Spade, Five ) ( Spade, Ace ) ( Spade, Three ) ( Spade, Two ) ( Spade, Four )
                            |> judgeHandRank
                            |> Expect.equal (StraightFlush 5)
                ]
            , describe
                "4カードのテスト"
                [ test "4カード: テストケース1" <|
                    \_ ->
                        makeHandWithoutSuit Ten Ten Two Ten Ten
                            |> judgeHandRank
                            |> Expect.equal (FourOfaKind 10 2)
                , test "4カード: テストケース2" <|
                    \_ ->
                        makeHandWithoutSuit Four Four Four Ace Four
                            |> judgeHandRank
                            |> Expect.equal (FourOfaKind 4 14)
                ]
            , describe
                "フルハウスのテスト"
                [ test "フルハウス: テストケース1" <|
                    \_ ->
                        makeHandWithoutSuit Two King King Two King
                            |> judgeHandRank
                            |> Expect.equal (FullHouse 13 2)
                , test "フルハウス: テストケース2" <|
                    \_ ->
                        makeHandWithoutSuit Six Jack Six Six Jack
                            |> judgeHandRank
                            |> Expect.equal (FullHouse 6 11)
                ]
            , describe "フラッシュのテスト"
                [ test "フラッシュ: テストケース1" <|
                    \_ ->
                        makeHand ( Spade, Ace ) ( Spade, Two ) ( Spade, King ) ( Spade, Queen ) ( Spade, Five )
                            |> judgeHandRank
                            |> Expect.equal (Flush 14 13 12 5 2)
                ]
            , describe "ストレートのテスト"
                [ test "ストレート: テストケース1" <|
                    \_ ->
                        makeHandWithoutSuit Three Four Six Seven Five
                            |> judgeHandRank
                            |> Expect.equal (Straight 7)
                , test "T,J,Q,K,Aのストレート" <|
                    \_ ->
                        makeHandWithoutSuit Ace Queen Jack King Ten
                            |> judgeHandRank
                            |> Expect.equal (Straight 14)
                , test "A,2,3,4,5のストレート" <|
                    \_ ->
                        makeHandWithoutSuit Three Five Ace Four Two
                            |> judgeHandRank
                            |> Expect.equal (Straight 5)
                ]
            , describe
                "3カードのテスト"
                [ test "3カード: テストケース1" <|
                    \_ ->
                        makeHandWithoutSuit Two Three Three Ace Three
                            |> judgeHandRank
                            |> Expect.equal (ThreeOfaKind 3 14 2)
                , test "3カード: テストケース2" <|
                    \_ ->
                        makeHandWithoutSuit Queen Seven Three Queen Queen
                            |> judgeHandRank
                            |> Expect.equal (ThreeOfaKind 12 7 3)
                , test "3カード: テストケース3" <|
                    \_ ->
                        makeHandWithoutSuit Five Five Ten Ace Five
                            |> judgeHandRank
                            |> Expect.equal (ThreeOfaKind 5 14 10)
                ]
            , describe
                "2ペアのテスト"
                [ test "2ペア: テストケース1" <|
                    \_ ->
                        makeHandWithoutSuit Five Five Three Ace Three
                            |> judgeHandRank
                            |> Expect.equal (TwoPair 5 3 14)
                , test "2ペア: テストケース2" <|
                    \_ ->
                        makeHandWithoutSuit Ace King King Ace Jack
                            |> judgeHandRank
                            |> Expect.equal (TwoPair 14 13 11)
                , test "2ペア: テストケース3" <|
                    \_ ->
                        makeHandWithoutSuit Ace Eight Eight Ace Nine
                            |> judgeHandRank
                            |> Expect.equal (TwoPair 14 8 9)
                ]
            , describe
                "1ペアのテスト"
                [ test "1ペア: テストケース1" <|
                    \_ ->
                        makeHandWithoutSuit Five Ace King Ace Three
                            |> judgeHandRank
                            |> Expect.equal (OnePair 14 13 5 3)
                , test "1ペア: テストケース2" <|
                    \_ ->
                        makeHandWithoutSuit Seven Ace King King Eight
                            |> judgeHandRank
                            |> Expect.equal (OnePair 13 14 8 7)
                , test "1ペア: テストケース3" <|
                    \_ ->
                        makeHandWithoutSuit Jack Ten King Ten Two
                            |> judgeHandRank
                            |> Expect.equal (OnePair 10 13 11 2)
                , test "1ペア: テストケース4" <|
                    \_ ->
                        makeHandWithoutSuit Three Ace King Jack Three
                            |> judgeHandRank
                            |> Expect.equal (OnePair 3 14 13 11)
                ]
            , describe
                "ハイカードのテスト"
                [ test "ハイカード: テストケース1" <|
                    \_ ->
                        makeHandWithoutSuit Five Ace King Six Three
                            |> judgeHandRank
                            |> Expect.equal (HighCard 14 13 6 5 3)
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
