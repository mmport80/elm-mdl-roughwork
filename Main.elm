module Main (..) where

import Router exposing (..)
import StartApp exposing (App)
import Effects exposing (Effects, Never)
import Html exposing (Html)
import Task exposing (Task)
import Material.Layout
import Material.Layout as Layout' exposing (defaultLayoutModel)
import Pages.Layout


init : ( Model, Effects Action )
init =
  ( Router.newModel, Effects.none )


taggedRouterSignal : Signal Action
taggedRouterSignal =
  Signal.map ApplyRoute router.signal


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = [ taggedRouterSignal ]
    }


inputs : List (Signal.Signal Pages.Layout.Action)
inputs =
  [ Layout'.setupSignals Pages.Layout.LayoutAction ]


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


port routeRunTask : Task () ()
port routeRunTask =
  router.run
