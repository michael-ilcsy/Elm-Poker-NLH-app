module Models.Deck exposing (Deck, emptyDeck, makeNewShuffledDeck, map)

import Models.Card as Card exposing (..)
import Models.Rank as Rank exposing (..)
import Models.Suit as Suit exposing (..)
import Random exposing (Generator)
import Random.List


suits =
    [ Spade, Hart, Club, Diamond ]


ranks =
    [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]


type Deck
    = Deck (List Card)


{-| ダミーの空Deck
-}
emptyDeck : Deck
emptyDeck =
    Deck []


{-| 新品のデッキを作ります
-}
makeNewDeck : Deck
makeNewDeck =
    Deck (suits |> List.concatMap (\suit -> ranks |> List.map (Card suit)))


shuffleDeck : Deck -> Generator Deck
shuffleDeck deck =
    deck
        |> toList
        |> Random.List.shuffle
        |> Random.map Deck


{-| シャッフルされた新品のデッキを作ります
-}
makeNewShuffledDeck : (Deck -> msg) -> Cmd msg
makeNewShuffledDeck msg =
    Random.generate msg <| shuffleDeck makeNewDeck


toList : Deck -> List Card
toList deck =
    case deck of
        Deck cardList ->
            cardList


map : (Card -> a) -> Deck -> List a
map f deck =
    case deck of
        Deck cardList ->
            cardList |> List.map f
