module Poker exposing (..)

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
