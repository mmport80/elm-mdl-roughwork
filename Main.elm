module Main (..) where

import Router exposing (router)
import StartApp exposing (App)
import Effects exposing (Effects, Never)
import Html exposing (Html)
import Task exposing (Task)
import Material.Layout
import Material.Layout as Layout' exposing (defaultLayoutModel)
import Pages.Layout


init : ( Router.Model, Effects Router.Action )
init =
  ( Router.model, Effects.none )


taggedRouterSignal : Signal Router.Action
taggedRouterSignal =
  Signal.map Router.ApplyRoute router.signal


app : StartApp.App Router.Model
app =
  StartApp.start
    { init = init
    , update = Router.update
    , view = Router.view
    , inputs =
        [ taggedRouterSignal ]
        --
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
