module Main exposing (..)

import Browser
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src)
import Models.Card exposing (..)
import Models.Deck as Deck exposing (makeNewShuffledDeck)



---- MODEL ----


type alias Model =
    { deck : List Card
    }


init : ( Model, Cmd Msg )
init =
    ( { deck = [] }, makeNewShuffledDeck InitDeck )



---- UPDATE ----


type Msg
    = InitDeck (List Card)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitDeck deck ->
            ( { model | deck = deck }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [] (model.deck |> List.map showCardImage)


{-| トランプのカードの画像を表示します
-}
showCardImage : Card -> Html msg
showCardImage card =
    img [ src (card |> toImagePath) ] []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
