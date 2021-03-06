module HandTest exposing (all)

import Expect exposing (Expectation)
import Models.Card exposing (Card)
import Models.Rank exposing (Rank(..))
import Models.Suit exposing (Suit(..))
import Poker.Hand exposing (..)
import Test exposing (..)


all : Test
all =
    describe "pokerのテスト"
        [ describe "役判定のテスト"
            [ describe "ロイヤルストレートフラッシュのテスト"
                [ test "ロイヤルストレートフラッシュ: テストケース1" <|
                    \_ ->
                        makeHand ( Spade, Ace ) ( Spade, Jack ) ( Spade, King ) ( Spade, Queen ) ( Spade, Ten )
                            |> judgeHandRankTest RoyalStraightFlush
                ]
            , describe "ストレートフラッシュのテスト"
                [ test "ストレートフラッシュ: テストケース1" <|
                    \_ ->
                        makeHand ( Spade, Nine ) ( Spade, Jack ) ( Spade, King ) ( Spade, Queen ) ( Spade, Ten )
                            |> judgeHandRankTest (StraightFlush 13)
                , test "ストレートフラッシュ: テストケース2" <|
                    \_ ->
                        makeHand ( Spade, Five ) ( Spade, Ace ) ( Spade, Three ) ( Spade, Two ) ( Spade, Four )
                            |> judgeHandRankTest (StraightFlush 5)
                ]
            , describe "4カードのテスト"
                [ test "4カード: テストケース1" <|
                    \_ ->
                        makeHandWithoutSuit Ten Ten Two Ten Ten
                            |> judgeHandRankTest (FourOfaKind 10 2)
                , test "4カード: テストケース2" <|
                    \_ ->
                        makeHandWithoutSuit Four Four Four Ace Four
                            |> judgeHandRankTest (FourOfaKind 4 14)
                ]
            , describe "フルハウスのテスト"
                [ test "フルハウス: テストケース1" <|
                    \_ ->
                        makeHandWithoutSuit Two King King Two King
                            |> judgeHandRankTest (FullHouse 13 2)
                , test "フルハウス: テストケース2" <|
                    \_ ->
                        makeHandWithoutSuit Six Jack Six Six Jack
                            |> judgeHandRankTest (FullHouse 6 11)
                ]
            , describe "フラッシュのテスト"
                [ test "フラッシュ: テストケース1" <|
                    \_ ->
                        makeHand ( Spade, Ace ) ( Spade, Two ) ( Spade, King ) ( Spade, Queen ) ( Spade, Five )
                            |> judgeHandRankTest (Flush 14 13 12 5 2)
                ]
            , describe "ストレートのテスト"
                [ test "ストレート: テストケース1" <|
                    \_ ->
                        makeHandWithoutSuit Three Four Six Seven Five
                            |> judgeHandRankTest (Straight 7)
                , test "T,J,Q,K,Aのストレート" <|
                    \_ ->
                        makeHandWithoutSuit Ace Queen Jack King Ten
                            |> judgeHandRankTest (Straight 14)
                , test "A,2,3,4,5のストレート" <|
                    \_ ->
                        makeHandWithoutSuit Three Five Ace Four Two
                            |> judgeHandRankTest (Straight 5)
                ]
            , describe "3カードのテスト"
                [ test "3カード: テストケース1" <|
                    \_ ->
                        makeHandWithoutSuit Two Three Three Ace Three
                            |> judgeHandRankTest (ThreeOfaKind 3 14 2)
                , test "3カード: テストケース2" <|
                    \_ ->
                        makeHandWithoutSuit Queen Seven Three Queen Queen
                            |> judgeHandRankTest (ThreeOfaKind 12 7 3)
                , test "3カード: テストケース3" <|
                    \_ ->
                        makeHandWithoutSuit Five Five Ten Ace Five
                            |> judgeHandRankTest (ThreeOfaKind 5 14 10)
                ]
            , describe "2ペアのテスト"
                [ test "2ペア: テストケース1" <|
                    \_ ->
                        makeHandWithoutSuit Five Five Three Ace Three
                            |> judgeHandRankTest (TwoPair 5 3 14)
                , test "2ペア: テストケース2" <|
                    \_ ->
                        makeHandWithoutSuit Ace King King Ace Jack
                            |> judgeHandRankTest (TwoPair 14 13 11)
                , test "2ペア: テストケース3" <|
                    \_ ->
                        makeHandWithoutSuit Ace Eight Eight Ace Nine
                            |> judgeHandRankTest (TwoPair 14 8 9)
                ]
            , describe "1ペアのテスト"
                [ test "1ペア: テストケース1" <|
                    \_ ->
                        makeHandWithoutSuit Five Ace King Ace Three
                            |> judgeHandRankTest (OnePair 14 13 5 3)
                , test "1ペア: テストケース2" <|
                    \_ ->
                        makeHandWithoutSuit Seven Ace King King Eight
                            |> judgeHandRankTest (OnePair 13 14 8 7)
                , test "1ペア: テストケース3" <|
                    \_ ->
                        makeHandWithoutSuit Jack Ten King Ten Two
                            |> judgeHandRankTest (OnePair 10 13 11 2)
                , test "1ペア: テストケース4" <|
                    \_ ->
                        makeHandWithoutSuit Three Ace King Jack Three
                            |> judgeHandRankTest (OnePair 3 14 13 11)
                ]
            , describe "ハイカードのテスト"
                [ test "ハイカード: テストケース1" <|
                    \_ ->
                        makeHandWithoutSuit Five Ace King Six Three
                            |> judgeHandRankTest (HighCard 14 13 6 5 3)
                ]
            ]
        , describe "役数値のテスト"
            [ test "ロイヤルストレートフラッシュの数値がRoyalStraightFlushに復元されること" <|
                \_ ->
                    RoyalStraightFlush
                        |> numberToHanRankTest
            , test "ストレートフラッシュの数値がStraightFlushに復元されること" <|
                \_ ->
                    StraightFlush 12
                        |> numberToHanRankTest
            , test "4カードの数値がFourOfaKindに復元されること" <|
                \_ ->
                    FourOfaKind 3 2
                        |> numberToHanRankTest
            , test "フルハウスの数値がFullHouseに復元されること" <|
                \_ ->
                    FullHouse 14 13
                        |> numberToHanRankTest
            , test "フラッシュの数値がFlushに復元されること" <|
                \_ ->
                    Flush 14 13 12 11 6
                        |> numberToHanRankTest
            , test "ストレートの数値がStraightに復元されること" <|
                \_ ->
                    Straight 8
                        |> numberToHanRankTest
            , test "3カードの数値がThreeOfaKindに復元されること" <|
                \_ ->
                    ThreeOfaKind 12 10 5
                        |> numberToHanRankTest
            , test "2ペアの数値がTwoPairに復元されること" <|
                \_ ->
                    TwoPair 14 9 3
                        |> numberToHanRankTest
            , test "1ペアの数値がOnePairに復元されること" <|
                \_ ->
                    OnePair 12 8 7 6
                        |> numberToHanRankTest
            , test "ハイカードの数値がHighCardに復元されること" <|
                \_ ->
                    HighCard 9 8 7 6 3
                        |> numberToHanRankTest
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


{-| 役判定関数をテストします
-}
judgeHandRankTest : HandRank -> Hand -> Expectation
judgeHandRankTest handRank hand =
    hand
        |> judgeHandRank
        |> Expect.equal handRank


{-| 役数値からHandRankに復元する関数をテストします
-}
numberToHanRankTest : HandRank -> Expectation
numberToHanRankTest handRank =
    handRank
        |> handRankToNumber
        |> numberToHandRank
        |> Expect.equal handRank
