module Parade.Types exposing (..)

import Dict exposing (Dict)

import Json.Decode exposing (..)

import Json.Decode.Pipeline exposing (..)

type alias Card =
    { color : Color
    , rank : Int
    }

type Color
    = Red
    | Green
    | Blue
    | Purple
    | Yellow
    | Black

type alias GameState =
    { playerIx : Int
    , players : List (Player)
    , numPlayers : Int
    , parade : List (Card)
    , deck : List (Card)
    , lastTurn : Maybe (Int)
    }

type alias Player =
    { hand : List (Card)
    , tableau : Dict (Int) (Dict (Int) (()))
    }

decodeCard : Decoder Card
decodeCard =
    decode Card
        |> required "color" decodeColor
        |> required "rank" int

decodeColor : Decoder Color
decodeColor =
    string |> andThen ( \x ->
        if x == "Red" then decode Red
        else if x == "Green" then decode Green
        else if x == "Blue" then decode Blue
        else if x == "Purple" then decode Purple
        else if x == "Yellow" then decode Yellow
        else if x == "Black" then decode Black
        else fail "Constructor not matched" )

decodeGameState : Decoder GameState
decodeGameState =
    decode GameState
        |> required "playerIx" int
        |> required "players" (list decodePlayer)
        |> required "numPlayers" int
        |> required "parade" (list decodeCard)
        |> required "deck" (list decodeCard)
        |> required "lastTurn" (maybe int)

decodePlayer : Decoder Player
decodePlayer =
    decode Player
        |> required "hand" (list decodeCard)
        |> required "tableau" (map Dict.fromList (list (map2 (,) (index 0 int) (index 1 (map Dict.fromList (list (map2 (,) (index 0 int) (index 1 (succeed ())))))))))