
import Task exposing (Task, sleep, andThen, succeed)

import Html exposing (..)
import Html.Events exposing (onClick)
import Effects exposing (Effects, Never)
import StartApp
import Clock


app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }


main =
  app.html


init =
  ( initialModel, Effects.none )


type alias Model =
  { clock : Clock.Model
  , clockIsRunning : Bool
  }


initialModel : Model
initialModel =
  { clock = 30
  , clockIsRunning = False
  }


type Action
  = NoOp
  | ToggleClock (Signal.Address Action)
  | ClockAction Clock.Action


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case Debug.log "main" action of
    NoOp ->
      ( model, Effects.none )

    ToggleClock address ->
      if model.clockIsRunning then
        ( { model | clockIsRunning = False }
        , Effects.none
        )
      else
        ( { model | clockIsRunning = True }
        , startCountdown address
        )

    ClockAction subAction ->
      ( { model | clock = Clock.update subAction model.clock }
      , Effects.none
      )


startCountdown address =
  Clock.countdown (Signal.forwardTo address ClockAction)
    |> Effects.task
    |> Effects.map (always NoOp)


view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ text <| "The time is " ++ (Clock.view model.clock)
    , button
        [ onClick address (ToggleClock address) ]
        [ text <| if model.clockIsRunning then "Stop timer" else "Start timer"
        ]
    , text <| toString model
    ]


port tasks : Signal (Task Never ())
port tasks =
  app.tasks
