module Poker exposing (..)

import Array
import Trump exposing (..)


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


{-| フラッシュかどうか調べて、HandRankを返します
-}
flush : Hand -> Maybe HandRank
flush hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                cardList =
                    [ card1, card2, card3, card4, card5 ]
            in
            if cardList |> isFlush then
                case
                    cardList
                        |> List.map toRankNumber
                        |> List.sort
                        |> List.reverse
                of
                    [ a, b, c, d, e ] ->
                        Just (Flush a b c d e)

                    _ ->
                        Nothing

            else
                Nothing


{-| フラッシュかどうか調べます
-}
isFlush : List Card -> Bool
isFlush cardList =
    case cardList of
        [ c1, c2, c3, c4, c5 ] ->
            cardList |> List.all (\card -> card.suit == c1.suit)

        _ ->
            False


{-| ストレートかどうか調べて、HandRankを返します
-}
straight : Hand -> Maybe HandRank
straight hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                cardRanks =
                    [ card1, card2, card3, card4, card5 ]
                        |> List.map toRankNumber
                        |> List.sort
                        |> List.reverse
            in
            if cardRanks == [ 14, 5, 4, 3, 2 ] then
                Just (Straight 5)

            else if cardRanks |> isStraight then
                case cardRanks of
                    [ a, b, c, d, e ] ->
                        Just (Straight a)

                    _ ->
                        Nothing

            else
                Nothing


{-| ストレートかどうか調べます
-}
isStraight : List Int -> Bool
isStraight list =
    case list of
        [ a, b, c, d, e ] ->
            ([ a, b, c, d, e ]
                |> List.map (\n -> n - e)
            )
                == [ 4, 3, 2, 1, 0 ]

        _ ->
            False


{-| ロイヤルストレートフラッシュかどうか調べて、HandRankを返します
-}
royalStraightFlush : Hand -> Maybe HandRank
royalStraightFlush hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                cardList =
                    [ card1, card2, card3, card4, card5 ]
            in
            if cardList |> isFlush then
                let
                    cardRanks =
                        cardList
                            |> List.map toRankNumber
                            |> List.sort
                            |> List.reverse
                in
                if cardRanks == [ 14, 13, 12, 11, 10 ] then
                    Just RoyalStraightFlush

                else
                    Nothing

            else
                Nothing


{-| ストレートフラッシュかどうか調べて、HandRankを返します
ロイヤルストレートフラッシュであったとしてもストレートフラッシュになります
-}
straightFlush : Hand -> Maybe HandRank
straightFlush hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                cardList =
                    [ card1, card2, card3, card4, card5 ]
            in
            if cardList |> isFlush then
                let
                    cardRanks =
                        cardList
                            |> List.map toRankNumber
                            |> List.sort
                            |> List.reverse
                in
                if cardRanks |> isStraight then
                    case cardRanks of
                        [ a, b, c, d, e ] ->
                            Just (StraightFlush a)

                        _ ->
                            Nothing

                else
                    Nothing

            else
                Nothing


{-| 4カードかどうか調べて、HandRankを返します
-}
fourOfaKind : Hand -> Maybe HandRank
fourOfaKind hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                cardRanks =
                    [ card1, card2, card3, card4, card5 ]
                        |> List.map toRankNumber
                        |> List.sort
                        |> List.reverse
            in
            case cardRanks of
                [ a, b, c, d, e ] ->
                    if a == b && b == c && c == d then
                        Just (FourOfaKind a e)

                    else if b == c && c == d && d == e then
                        Just (FourOfaKind b a)

                    else
                        Nothing

                _ ->
                    Nothing


{-| フルハウスかどうか調べて、HandRankを返します
-}
fullHouse : Hand -> Maybe HandRank
fullHouse hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                cardRanks =
                    [ card1, card2, card3, card4, card5 ]
                        |> List.map toRankNumber
                        |> List.sort
                        |> List.reverse
            in
            case cardRanks of
                [ a, b, c, d, e ] ->
                    if a == b && b == c && d == e then
                        Just (FullHouse a d)

                    else if a == b && c == d && d == e then
                        Just (FullHouse c a)

                    else
                        Nothing

                _ ->
                    Nothing


{-| 3カードかどうか調べて、HandRankを返します
-}
threeOfaKind : Hand -> Maybe HandRank
threeOfaKind hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                cardRanks =
                    [ card1, card2, card3, card4, card5 ]
                        |> List.map toRankNumber
                        |> List.sort
                        |> List.reverse
            in
            case cardRanks of
                [ a, b, c, d, e ] ->
                    if a == b && b == c then
                        Just (ThreeOfaKind a d e)

                    else if b == c && c == d then
                        Just (ThreeOfaKind b a e)

                    else if c == d && d == e then
                        Just (ThreeOfaKind c a b)

                    else
                        Nothing

                _ ->
                    Nothing


{-| 2ペアかどうか調べて、HandRankを返します
-}
twoPair : Hand -> Maybe HandRank
twoPair hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                cardRanks =
                    [ card1, card2, card3, card4, card5 ]
                        |> List.map toRankNumber
                        |> List.sort
                        |> List.reverse
            in
            case cardRanks of
                [ a, b, c, d, e ] ->
                    if a == b && c == d then
                        Just (TwoPair a c e)

                    else if a == b && d == e then
                        Just (TwoPair a d c)

                    else if b == c && d == e then
                        Just (TwoPair b d a)

                    else
                        Nothing

                _ ->
                    Nothing


{-| 1ペアかどうか調べて、HandRankを返します
-}
onePair : Hand -> Maybe HandRank
onePair hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                cardRanks =
                    [ card1, card2, card3, card4, card5 ]
                        |> List.map toRankNumber
                        |> List.sort
                        |> List.reverse
            in
            case cardRanks of
                [ a, b, c, d, e ] ->
                    if a == b then
                        Just (OnePair a c d e)

                    else if b == c then
                        Just (OnePair b a d e)

                    else if c == d then
                        Just (OnePair c a b e)

                    else if d == e then
                        Just (OnePair d a b c)

                    else
                        Nothing

                _ ->
                    Nothing


{-| ハイカードのHandRankを返します
-}
highCard : Hand -> HandRank
highCard hand =
    case hand of
        Hand card1 card2 card3 card4 card5 ->
            let
                cardRanksArray =
                    [ card1, card2, card3, card4, card5 ]
                        |> List.map toRankNumber
                        |> List.sort
                        |> List.reverse
                        |> Array.fromList

                getRankNumberFromCardRanksArray int =
                    cardRanksArray |> Array.get int |> Maybe.withDefault 0

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
            in
            HighCard r1 r2 r3 r4 r5
