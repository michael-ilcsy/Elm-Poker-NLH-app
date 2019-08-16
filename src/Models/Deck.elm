module Models.Deck exposing (Deck, dummyDeck, makeNewShuffledDeck, map)

import Models.Card as Card exposing (..)
import Models.Rank as Rank exposing (..)
import Models.Suit as Suit exposing (..)
import Poker exposing (..)
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
dummyDeck : Deck
dummyDeck =
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


{-| デッキからカードを配ります
-}
deal : Deck -> ( Board, PlayerHand, PlayerHand )
deal deck =
    case deck of
        Deck cardList ->
            case cardList of
                c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: rest ->
                    ( Board c1 c2 c3 c4 c5
                    , PlayerHand c6 c7
                    , PlayerHand c8 c9
                    )

                _ ->
                    ( dummyBoard
                    , dummyPlayerHand
                    , dummyPlayerHand
                    )


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
