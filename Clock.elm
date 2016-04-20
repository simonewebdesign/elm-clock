module Clock (Model, init, view, Action, update, tick, tickInterval) where

import Task exposing (Task, sleep, andThen)
import Time exposing (Time)


type alias Model = Int


init : Model
init =
  0


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
