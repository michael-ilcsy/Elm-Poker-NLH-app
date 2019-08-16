module Models.Card exposing (..)

import Models.Rank as Rank exposing (Rank(..), toNumber)
import Models.Suit as Suit exposing (Suit(..))


type alias Card =
    { suit : Suit
    , rank : Rank
    }


{-| ダミーのCard
-}
dummyCard : Card
dummyCard =
    Card Spade Ace


{-| CardからRankの表す数値に変換します
-}
toRankNumber : Card -> Int
toRankNumber card =
    card.rank |> toNumber


{-| Cardから画像のパスを生成します
-}
toImagePath : Card -> String
toImagePath card =
    "%PUBLIC_URL%/img/" ++ (card.suit |> Suit.toString) ++ (card.rank |> Rank.toString) ++ ".png"
