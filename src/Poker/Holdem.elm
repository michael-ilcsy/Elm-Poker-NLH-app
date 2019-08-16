module Poker.Holdem exposing (..)

import Models.Card exposing (..)
import Models.Deck exposing (..)
import Poker.Hand exposing (..)


type Board
    = Board Card Card Card Card Card


{-| ダミーのBoard
-}
dummyBoard : Board
dummyBoard =
    Board dummyCard dummyCard dummyCard dummyCard dummyCard


type PlayerHand
    = PlayerHand Card Card


{-| ダミーのPlayerHand
-}
dummyPlayerHand : PlayerHand
dummyPlayerHand =
    PlayerHand dummyCard dummyCard


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


mapBoard : (Card -> a) -> Board -> List a
mapBoard f board =
    case board of
        Board c1 c2 c3 c4 c5 ->
            [ c1, c2, c3, c4, c5 ] |> List.map f


mapPlayerHand : (Card -> a) -> PlayerHand -> List a
mapPlayerHand f playerHand =
    case playerHand of
        PlayerHand c1 c2 ->
            [ c1, c2 ] |> List.map f


initializeHoldemGame : Deck -> ( Board, PlayerHand, PlayerHand )
initializeHoldemGame deck =
    deck |> deal


{-| BoardとPlayerHandから21通りのHandのListを生成します
-}
generateHandList : Board -> PlayerHand -> List Hand
generateHandList board playerHand =
    case board of
        Board c1 c2 c3 c4 c5 ->
            case playerHand of
                PlayerHand c6 c7 ->
                    [ Hand c1 c2 c3 c4 c5
                    , Hand c1 c2 c3 c4 c6
                    , Hand c1 c2 c3 c4 c7
                    , Hand c1 c2 c3 c5 c6
                    , Hand c1 c2 c3 c5 c7
                    , Hand c1 c2 c3 c6 c7
                    , Hand c1 c2 c4 c5 c6
                    , Hand c1 c2 c4 c5 c7
                    , Hand c1 c2 c4 c6 c7
                    , Hand c1 c2 c5 c6 c7
                    , Hand c1 c3 c4 c5 c6
                    , Hand c1 c3 c4 c5 c7
                    , Hand c1 c3 c4 c6 c7
                    , Hand c1 c3 c5 c6 c7
                    , Hand c1 c4 c5 c6 c7
                    , Hand c2 c3 c4 c5 c6
                    , Hand c2 c3 c4 c5 c7
                    , Hand c2 c3 c4 c6 c7
                    , Hand c2 c3 c5 c6 c7
                    , Hand c2 c4 c5 c6 c7
                    , Hand c3 c4 c5 c6 c7
                    ]
