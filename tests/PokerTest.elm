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
                [ test "ロイヤルストレートフラッシュのときにRoyalStraightFlushが返ること" <|
                    \_ ->
                        makeHand ( Spade, Ace ) ( Spade, Jack ) ( Spade, King ) ( Spade, Queen ) ( Spade, Ten )
                            |> judgeHandRank
                            |> Expect.equal RoyalStraightFlush
                ]
            , describe
                "ストレートフラッシュのテスト"
                [ test "ストレートフラッシュのときにStraightFlushが返ること" <|
                    \_ ->
                        makeHand ( Spade, Nine ) ( Spade, Jack ) ( Spade, King ) ( Spade, Queen ) ( Spade, Ten )
                            |> judgeHandRank
                            |> Expect.equal (StraightFlush 13)
                ]
            , describe
                "4カードのテスト"
                [ test "4カードのときにFourOfaKindが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Ten Ten Two Ten Ten
                            |> judgeHandRank
                            |> Expect.equal (FourOfaKind 10 2)
                ]
            , describe
                "フルハウスのテスト"
                [ test "フルハウスのときにFullHouseが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Two King King Two King
                            |> judgeHandRank
                            |> Expect.equal (FullHouse 13 2)
                ]
            , describe "フラッシュのテスト"
                [ test "フラッシュのときにFlushが返ること" <|
                    \_ ->
                        makeHand ( Spade, Ace ) ( Spade, Two ) ( Spade, King ) ( Spade, Queen ) ( Spade, Five )
                            |> judgeHandRank
                            |> Expect.equal (Flush 14 13 12 5 2)
                ]
            , describe "ストレートのテスト"
                [ test "ストレートのときにStraightが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Three Four Six Seven Five
                            |> judgeHandRank
                            |> Expect.equal (Straight 7)
                , test "T,J,Q,K,AのストレートのときにStraightが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Ace Queen Jack King Ten
                            |> judgeHandRank
                            |> Expect.equal (Straight 14)
                , test "A,2,3,4,5のストレートのときにStraightが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Three Five Ace Four Two
                            |> judgeHandRank
                            |> Expect.equal (Straight 5)
                ]
            , describe
                "3カードのテスト"
                [ test "3カードのときにThreeOfaKindが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Two Three Three Ace Three
                            |> judgeHandRank
                            |> Expect.equal (ThreeOfaKind 3 14 2)
                ]
            , describe
                "2ペアのテスト"
                [ test "2ペアのときにTwoPairが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Five Five Three Ace Three
                            |> judgeHandRank
                            |> Expect.equal (TwoPair 5 3 14)
                ]
            , describe
                "1ペアのテスト"
                [ test "1ペアのときにOnePairが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Five Ace King Ace Three
                            |> judgeHandRank
                            |> Expect.equal (OnePair 14 13 5 3)
                ]
            , describe
                "ハイカードのテスト"
                [ test "役がないときにHighCardが返ること" <|
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
