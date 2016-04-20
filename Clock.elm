module Clock where

import Graphics.Element exposing (show)
import Graphics.Collage exposing (..)
import Graphics.Input exposing (clickable)
import Text
import Task exposing (Task, sleep, andThen, succeed)
import Time exposing (Time)

--import Task exposing (Task, sleep, andThen, succeed)

--main =
--  Signal.map (view actions.address) model


type alias Model = Int


initialModel : Model
initialModel =
  0

--view : Signal.Address Action -> Model -> String
--view address model =
--  timeView model


view : Model -> String
view model =
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
  case Debug.log "clock" action of
    Start ->
      model

    Tick ->
      model + 1


--actions : Signal.Mailbox Action
--actions =
--  Signal.mailbox Start


--model : Signal Model
--model =
--  Signal.foldp update initialModel actions.signal

tick : Signal.Address Action -> Task x ()
tick address =
  sleep 1000
  `andThen` \_ -> Signal.send address Tick
  `andThen` (always (tick address))


tickInterval : Signal.Address Action -> Time -> Task x ()
tickInterval address interval =
  sleep interval
  `andThen` \_ -> Signal.send address Tick
  `andThen` (always (tickInterval address interval))


--tasksMailbox : Signal.Mailbox (Task x ())
--tasksMailbox =
--  Signal.mailbox (Task.succeed ())


--port tasks : Signal (Task x ())
--port tasks =
--  tasksMailbox.signal
