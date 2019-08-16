module Models.Deck exposing (..)

import Models.Card as Card exposing (..)
import Models.Rank as Rank exposing (..)
import Models.Suit as Suit exposing (..)
import Random exposing (Generator)
import Random.List


suits =
    [ Spade, Hart, Club, Diamond ]


ranks =
    [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]


{-| 新品のデッキを作ります
-}
makeNewDeck : List Card
makeNewDeck =
    suits |> List.concatMap (\suit -> ranks |> List.map (Card suit))


shuffleDeck : List Card -> Generator (List Card)
shuffleDeck deck =
    Random.List.shuffle deck


{-| シャッフルされた新品のデッキを作ります
-}
makeNewShuffledDeck : (List Card -> msg) -> Cmd msg
makeNewShuffledDeck msg =
    Random.generate msg <| shuffleDeck makeNewDeck
