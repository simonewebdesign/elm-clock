module Clock where

import Graphics.Element exposing (show)
import Graphics.Collage exposing (..)
import Graphics.Input exposing (clickable)
import Text

import Task exposing (Task, sleep, andThen, succeed)

main =
  Signal.map (view actions.address) model


type alias Model = Int


view : Signal.Address Action -> Model -> Graphics.Element.Element
view address model =
  collage 300 200
    [ show (toString model)
        |> toForm

    , Text.fromString (timeView model)
        |> Text.italic
        |> text
        |> moveY -20
    ]
  |> clickable (Signal.message tasksMailbox.address tick)


timeView : Model -> String
timeView model =
  let
    seconds =
      let
        secs = model % 60
      in
        if secs < 10 then -- add leading zero
          "0" ++ (toString secs)
        else
          toString secs

    minutes =
      let
        mins = (floor ((toFloat model) / 60.0)) % 60
      in
        if mins < 10 then -- add leading zero
          "0" ++ (toString mins)
        else
          toString mins
  in
    minutes ++ ":" ++ seconds


type Action
  = Start
  | Tick


update : Action -> Model -> Model
update action model =
  case action of
    Start ->
      model

    Tick ->
      model + 1


actions : Signal.Mailbox Action
actions =
  Signal.mailbox Start


tasksMailbox : Signal.Mailbox (Task x ())
tasksMailbox =
  Signal.mailbox (Task.succeed ())


initialModel : Model
initialModel =
  0


model : Signal Model
model =
  Signal.foldp update initialModel actions.signal


tick : Task x ()
tick =
  sleep 1000
  `andThen` \_ -> Signal.send actions.address Tick
  `andThen` (always tick)


port tasks : Signal (Task x ())
port tasks =
  tasksMailbox.signal
