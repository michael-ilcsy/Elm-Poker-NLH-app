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
                            |> royalStraightFlush
                            |> Expect.equal (Just RoyalStraightFlush)
                , test "ロイヤルストレートフラッシュじゃないときにNothingが返ること" <|
                    \_ ->
                        makeHand ( Spade, Jack ) ( Spade, Ten ) ( Spade, King ) ( Spade, Queen ) ( Spade, Nine )
                            |> royalStraightFlush
                            |> Expect.equal Nothing
                ]
            , describe
                "ストレートフラッシュのテスト"
                [ test "ストレートフラッシュのときにStraightFlushが返ること" <|
                    \_ ->
                        makeHand ( Spade, Nine ) ( Spade, Jack ) ( Spade, King ) ( Spade, Queen ) ( Spade, Ten )
                            |> straightFlush
                            |> Expect.equal (Just (StraightFlush 13))
                , test "ストレートフラッシュじゃないときにNothingが返ること" <|
                    \_ ->
                        makeHand ( Spade, Jack ) ( Spade, Ten ) ( Spade, King ) ( Spade, Queen ) ( Spade, Two )
                            |> straightFlush
                            |> Expect.equal Nothing
                ]
            , describe
                "4カードのテスト"
                [ test "4カードのときにFourOfaKindが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Ten Ten Two Ten Ten
                            |> fourOfaKind
                            |> Expect.equal (Just (FourOfaKind 10 2))
                , test "4カードじゃないときにNothingが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Two King King Two King
                            |> fourOfaKind
                            |> Expect.equal Nothing
                ]
            , describe
                "フルハウスのテスト"
                [ test "フルハウスのときにFullHouseが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Two King King Two King
                            |> fullHouse
                            |> Expect.equal (Just (FullHouse 13 2))
                , test "フルハウスじゃないときにNothingが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Two King King Two Ace
                            |> fullHouse
                            |> Expect.equal Nothing
                ]
            , describe "フラッシュのテスト"
                [ test "フラッシュのときにFlushが返ること" <|
                    \_ ->
                        makeHand ( Spade, Ace ) ( Spade, Two ) ( Spade, King ) ( Spade, Queen ) ( Spade, Five )
                            |> flush
                            |> Expect.equal (Just (Flush 14 13 12 5 2))
                , test "フラッシュじゃないときにNothingが返ること" <|
                    \_ ->
                        makeHand ( Spade, Ace ) ( Diamond, Two ) ( Spade, King ) ( Spade, Queen ) ( Spade, Five )
                            |> flush
                            |> Expect.equal Nothing
                ]
            , describe "ストレートのテスト"
                [ test "ストレートのときにStraightが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Three Four Six Seven Five
                            |> straight
                            |> Expect.equal (Just (Straight 7))
                , test "T,J,Q,K,AのストレートのときにStraightが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Ace Queen Jack King Ten
                            |> straight
                            |> Expect.equal (Just (Straight 14))
                , test "A,2,3,4,5のストレートのときにStraightが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Three Five Ace Four Two
                            |> straight
                            |> Expect.equal (Just (Straight 5))
                , test "ストレートじゃないときにNothingが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Ten Nine Eight Five Seven
                            |> straight
                            |> Expect.equal Nothing
                ]
            , describe
                "3カードのテスト"
                [ test "3カードのときにThreeOfaKindが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Two Three Three Ace Three
                            |> threeOfaKind
                            |> Expect.equal (Just (ThreeOfaKind 3 14 2))
                , test "3カードじゃないときにNothingが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Two King King Two Ace
                            |> threeOfaKind
                            |> Expect.equal Nothing
                ]
            , describe
                "2ペアのテスト"
                [ test "2ペアのときにTwoPairが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Five Five Three Ace Three
                            |> twoPair
                            |> Expect.equal (Just (TwoPair 5 3 14))
                , test "2ペアじゃないときにNothingが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Two King King Nine Ace
                            |> twoPair
                            |> Expect.equal Nothing
                ]
            , describe
                "1ペアのテスト"
                [ test "1ペアのときにOnePairが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Five Ace King Ace Three
                            |> onePair
                            |> Expect.equal (Just (OnePair 14 13 5 3))
                , test "1ペアじゃないときにNothingが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Two King Jack Nine Ace
                            |> onePair
                            |> Expect.equal Nothing
                ]
            , describe
                "ハイカードのテスト"
                [ test "役がないときにHighCardが返ること" <|
                    \_ ->
                        makeHandWithoutSuit Five Ace King Six Three
                            |> highCard
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
