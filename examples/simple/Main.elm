
import Task exposing (Task, sleep, andThen, succeed)

import Html exposing (..)
import Html.Events exposing (onClick)

import Clock


main =
  Signal.map (view actions.address) model


type alias Model =
  { clock : Clock.Model
  , clockIsRunning : Bool
  }


initialModel : Model
initialModel =
  { clock = Clock.initialModel
  , clockIsRunning = False
  }


type Action
  = NoOp
  --= ToggleClock
  | ClockAction Clock.Action


update : Action -> Model -> Model
update action model =
  case Debug.log "main" action of
    NoOp ->
      model

    --StartClock ->
    --  ( { model | clockIsRunning = True }
    --  ,
    --  )

    --ToggleClock ->
    --  ( { model | clockIsRunning = not model.clockIsRunning }
    --  , Effects.none
    --  )

    ClockAction subAction ->
      { model | clock = Clock.update subAction model.clock }


view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ text <| "The time is " ++ (Clock.view model.clock)
    , button
        --[]
        [ onClick tasksMailbox.address tick ]
        --[ onClick address StartClock ]
        --[ onClick address ToggleClock ]
        [ text <| if model.clockIsRunning then "Stop timer" else "Start timer" ]
    , text <| toString model
    ]


tick : Task x ()
tick =
  let
    _ = Debug.log "tick" ()
  in
    sleep 1000
    `andThen` \_ -> Signal.send actions.address (ClockAction Clock.Tick)
    `andThen` (always tick)


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


model : Signal Model
model =
  Signal.foldp update initialModel actions.signal


tasksMailbox : Signal.Mailbox (Task x ())
tasksMailbox =
  Signal.mailbox (Task.succeed ())


port tasks : Signal (Task x ())
port tasks =
  tasksMailbox.signal
