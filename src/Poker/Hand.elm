module Poker.Hand exposing (Hand(..), HandRank(..), handRankToNumber, handRankToString, judgeHandRank, numberToHandRank)

import Array
import Models.Card exposing (Card, toRankNumber)


type Hand
    = Hand Card Card Card Card Card


type HandRank
    = RoyalStraightFlush
    | StraightFlush Int
    | FourOfaKind Int Int
    | FullHouse Int Int
    | Flush Int Int Int Int Int
    | Straight Int
    | ThreeOfaKind Int Int Int
    | TwoPair Int Int Int
    | OnePair Int Int Int Int
    | HighCard Int Int Int Int Int


judgeHandRank : Hand -> HandRank
judgeHandRank hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                cardList =
                    [ card1, card2, card3, card4, card5 ]

                cardRanks =
                    cardList
                        |> List.map toRankNumber
                        |> List.sort
                        |> List.reverse

                getRankNumberFromCardRanksArray int =
                    cardRanks |> Array.fromList |> Array.get int |> Maybe.withDefault 0

                r1 =
                    getRankNumberFromCardRanksArray 0

                r2 =
                    getRankNumberFromCardRanksArray 1

                r3 =
                    getRankNumberFromCardRanksArray 2

                r4 =
                    getRankNumberFromCardRanksArray 3

                r5 =
                    getRankNumberFromCardRanksArray 4

                isFlush =
                    cardList |> List.all (\card -> card.suit == card1.suit)

                isStraight =
                    (cardRanks
                        |> List.map (\n -> n - r5)
                    )
                        == [ 4, 3, 2, 1, 0 ]

                isFiveHighStraight =
                    cardRanks == [ 14, 5, 4, 3, 2 ]
            in
            if isFlush && cardRanks == [ 14, 13, 12, 11, 10 ] then
                RoyalStraightFlush

            else if isFlush && isStraight then
                StraightFlush r1

            else if isFlush && isFiveHighStraight then
                StraightFlush 5

            else if r1 == r2 && r2 == r3 && r3 == r4 then
                FourOfaKind r1 r5

            else if r2 == r3 && r3 == r4 && r4 == r5 then
                FourOfaKind r2 r1

            else if r1 == r2 && r2 == r3 && r4 == r5 then
                FullHouse r1 r4

            else if r1 == r2 && r3 == r4 && r4 == r5 then
                FullHouse r3 r1

            else if isFlush then
                Flush r1 r2 r3 r4 r5

            else if isStraight then
                Straight r1

            else if isFiveHighStraight then
                Straight 5

            else if r1 == r2 && r2 == r3 then
                ThreeOfaKind r1 r4 r5

            else if r2 == r3 && r3 == r4 then
                ThreeOfaKind r2 r1 r5

            else if r3 == r4 && r4 == r5 then
                ThreeOfaKind r3 r1 r2

            else if r1 == r2 && r3 == r4 then
                TwoPair r1 r3 r5

            else if r1 == r2 && r4 == r5 then
                TwoPair r1 r4 r3

            else if r2 == r3 && r4 == r5 then
                TwoPair r2 r4 r1

            else if r1 == r2 then
                OnePair r1 r3 r4 r5

            else if r2 == r3 then
                OnePair r2 r1 r4 r5

            else if r3 == r4 then
                OnePair r3 r1 r2 r5

            else if r4 == r5 then
                OnePair r4 r1 r2 r3

            else
                HighCard r1 r2 r3 r4 r5


handRankToString : HandRank -> String
handRankToString handRank =
    case handRank of
        RoyalStraightFlush ->
            "ロイヤルストレートフラッシュ"

        StraightFlush r1 ->
            (r1 |> handRankToStringHelp) ++ "ハイ ストレートフラッシュ"

        FourOfaKind r1 r2 ->
            "4カード (" ++ handRankToStringHelp2 r1 r1 r1 r1 r2 ++ ")"

        FullHouse r1 r2 ->
            "フルハウス (" ++ handRankToStringHelp2 r1 r1 r1 r2 r2 ++ ")"

        Flush r1 r2 r3 r4 r5 ->
            "フラッシュ (" ++ handRankToStringHelp2 r1 r2 r3 r4 r5 ++ ")"

        Straight r1 ->
            (r1 |> handRankToStringHelp) ++ "ハイ ストレート"

        ThreeOfaKind r1 r2 r3 ->
            "3カード (" ++ handRankToStringHelp2 r1 r1 r1 r2 r3 ++ ")"

        TwoPair r1 r2 r3 ->
            "2ペア (" ++ handRankToStringHelp2 r1 r1 r2 r2 r3 ++ ")"

        OnePair r1 r2 r3 r4 ->
            "1ペア (" ++ handRankToStringHelp2 r1 r1 r2 r3 r4 ++ ")"

        HighCard r1 r2 r3 r4 r5 ->
            "ハイカード (" ++ handRankToStringHelp2 r1 r2 r3 r4 r5 ++ ")"


handRankToStringHelp : Int -> String
handRankToStringHelp int =
    case int of
        14 ->
            "A"

        13 ->
            "K"

        12 ->
            "Q"

        11 ->
            "J"

        10 ->
            "T"

        _ ->
            String.fromInt int


handRankToStringHelp2 : Int -> Int -> Int -> Int -> Int -> String
handRankToStringHelp2 r1 r2 r3 r4 r5 =
    [ r1, r2, r3, r4, r5 ]
        |> List.map handRankToStringHelp
        |> String.join ""


{-| RankBase(役ごとに決まる数値のベース)
-}
rb =
    10 ^ 7


{-| KickerBase(キッカーごとに決まる数値のベース)
-}
kb =
    20


kb1 =
    kb ^ 4


kb2 =
    kb ^ 3


kb3 =
    kb ^ 2


kb4 =
    kb


{-| HandRankを役の強さに合わせて数値にします
-}
handRankToNumber : HandRank -> Int
handRankToNumber handRank =
    case handRank of
        HighCard r1 r2 r3 r4 r5 ->
            (r1 * kb1) + (r2 * kb2) + (r3 * kb3) + (r4 * kb4) + r5

        OnePair r1 r2 r3 r4 ->
            rb + (r1 * kb1) + (r2 * kb2) + (r3 * kb3) + (r4 * kb4)

        TwoPair r1 r2 r3 ->
            rb * 2 + (r1 * kb1) + (r2 * kb2) + (r3 * kb3)

        ThreeOfaKind r1 r2 r3 ->
            rb * 3 + (r1 * kb1) + (r2 * kb2) + (r3 * kb3)

        Straight r1 ->
            rb * 4 + (r1 * kb1)

        Flush r1 r2 r3 r4 r5 ->
            rb * 5 + (r1 * kb1) + (r2 * kb2) + (r3 * kb3) + (r4 * kb4) + r5

        FullHouse r1 r2 ->
            rb * 6 + (r1 * kb1) + (r2 * kb2)

        FourOfaKind r1 r2 ->
            rb * 7 + (r1 * kb1) + (r2 * kb2)

        StraightFlush r1 ->
            rb * 8 + (r1 * kb1)

        RoyalStraightFlush ->
            rb * 9


numberToHandRank : Int -> HandRank
numberToHandRank int =
    if int // (rb * 9) == 1 then
        RoyalStraightFlush

    else if int // (rb * 8) == 1 then
        let
            r1 =
                (int - (rb * 8)) // kb1
        in
        StraightFlush r1

    else if int // (rb * 7) == 1 then
        let
            numberWithoutRb =
                int - (rb * 7)

            { r1, r2 } =
                numberToHandRankHelp numberWithoutRb
        in
        FourOfaKind r1 r2

    else if int // (rb * 6) == 1 then
        let
            numberWithoutRb =
                int - (rb * 6)

            { r1, r2 } =
                numberToHandRankHelp numberWithoutRb
        in
        FullHouse r1 r2

    else if int // (rb * 5) == 1 then
        let
            numberWithoutRb =
                int - (rb * 5)

            { r1, r2, r3, r4, r5 } =
                numberToHandRankHelp numberWithoutRb
        in
        Flush r1 r2 r3 r4 r5

    else if int // (rb * 4) == 1 then
        let
            r1 =
                (int - (rb * 4)) // kb1
        in
        Straight r1

    else if int // (rb * 3) == 1 then
        let
            numberWithoutRb =
                int - (rb * 3)

            { r1, r2, r3 } =
                numberToHandRankHelp numberWithoutRb
        in
        ThreeOfaKind r1 r2 r3

    else if int // (rb * 2) == 1 then
        let
            numberWithoutRb =
                int - (rb * 2)

            { r1, r2, r3 } =
                numberToHandRankHelp numberWithoutRb
        in
        TwoPair r1 r2 r3

    else if int // (rb * 1) == 1 then
        let
            numberWithoutRb =
                int - (rb * 1)

            { r1, r2, r3, r4 } =
                numberToHandRankHelp numberWithoutRb
        in
        OnePair r1 r2 r3 r4

    else
        let
            { r1, r2, r3, r4, r5 } =
                numberToHandRankHelp int
        in
        HighCard r1 r2 r3 r4 r5


numberToHandRankHelp : Int -> { r1 : Int, r2 : Int, r3 : Int, r4 : Int, r5 : Int }
numberToHandRankHelp int =
    let
        r1 =
            int // kb1

        r2 =
            (int - r1 * kb1) // kb2

        r3 =
            (int - r1 * kb1 - r2 * kb2) // kb3

        r4 =
            (int - r1 * kb1 - r2 * kb2 - r3 * kb3) // kb4

        r5 =
            int - r1 * kb1 - r2 * kb2 - r3 * kb3 - r4 * kb4
    in
    { r1 = r1, r2 = r2, r3 = r3, r4 = r4, r5 = r5 }
