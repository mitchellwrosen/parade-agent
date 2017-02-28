import Html exposing (..)

import Parade.Types

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

--------------------------------------------------------------------------------
-- Model

type alias Model = ()

init : (Model, Cmd Msg)
init =
  ((), Cmd.none)


--------------------------------------------------------------------------------
-- Update

type Msg
  = Noop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)

--------------------------------------------------------------------------------
-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

--------------------------------------------------------------------------------
-- View

view : Model -> Html Msg
view model =
  div [] [ text "Hello" ]
