module Clock (Model, init, init', view, Action, update, tick, tickInterval, countdown) where

import Signal exposing (Address)
import Task exposing (Task, sleep, andThen)
import Time exposing (Time)


type alias Model = Int


init : Model
init =
  0


-- A double init might be another good reason to split this module into two.
-- Although the reason for it is just to be able to override the default.
init' : Model -> Model
init' model =
  model


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
  | Tock


update : Action -> Model -> Model
update action model =
  case Debug.log "clock" action of
    Start ->
      model

    Tick ->
      model + 1

    Tock ->
      if model > 0 then
        model - 1
      else
        model


tick : Address Action -> Task x ()
tick address =
  sleep 1000
  `andThen` \_ -> Signal.send address Tick
  `andThen` (always (tick address))


tickInterval : Address Action -> Time -> Task x ()
tickInterval address interval =
  sleep interval
  `andThen` \_ -> Signal.send address Tick
  `andThen` (always (tickInterval address interval))


-- It's in the same module because fundamentally it's also the same Model (Int).
-- However we should consider a separate Countdown module, because we're going
-- to have to handle the case where the countdown has finished and we don't
-- have such case in a normal Clock/Timer.
countdown : Address Action -> Task x ()
countdown address =
  let _ = Debug.log "cd" () in
  sleep 1000
  `andThen` \_ -> Signal.send address Tock
  `andThen` (always (countdown address))
