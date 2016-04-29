module Pages.Layout (..) where

-- import StartApp

import Html exposing (..)
import Html.Attributes exposing (href, class, style, key)
import Signal exposing (Signal)
import Effects exposing (..)
import Signal


-- import Task exposing (Task)

import Array exposing (Array)
import Material.Color as Color
import Material.Layout
import Material.Layout as Layout exposing (defaultLayoutModel)
import Material.Helpers exposing (lift, lift')
import Material.Style as Style
import Material.Scheme as Scheme
import Pages.Login


-- MODEL


layoutModel : Layout.Model
layoutModel =
  defaultLayoutModel



-- { defaultLayoutModel
--   | state = Layout.initState (List.length tabs)
-- }


type alias Model =
  { layout : Layout.Model
  , loginPage : Pages.Login.Model
  }


model : Model
model =
  { layout = layoutModel
  , loginPage = Pages.Login.model
  }



-- ACTION, UPDATE


type Action
  = LayoutAction Layout.Action
  | LoginPage Pages.Login.Action


nth : Int -> List a -> Maybe a
nth k xs =
  List.drop k xs |> List.head


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case Debug.log "Action" action of
    LayoutAction a ->
      let
        ( lifted, layoutFx ) =
          lift .layout (\m x -> { m | layout = x }) LayoutAction Layout.update a model
      in
        ( lifted, Effects.batch [ layoutFx ] )

    LoginPage input ->
      let
        ( login, fx ) =
          Pages.Login.update input model.loginPage

        model' =
          { model | loginPage = login }
      in
        ( model', Effects.map LoginPage fx )



-- VIEW


type alias Addr =
  Signal.Address Action


drawer : List Html
drawer =
  [ Layout.title "Menu"
  , Layout.navigation
      [ Layout.link
          [ href "https://github.com/debois/elm-mdl" ]
          [ text "Tool 1" ]
      , Layout.link
          [ href "http://package.elm-lang.org/packages/debois/elm-mdl/latest/" ]
          [ text "Tool 2" ]
      , Layout.link
          [ href "http://package.elm-lang.org/packages/debois/elm-mdl/latest/" ]
          [ text "Tool 3" ]
      ]
  ]


header : List Html
header =
  [ Layout.row
      [ Layout.title "Mission Control"
      , Layout.spacer
      , Layout.navigation
          [ Layout.link
              [ href "http://package.elm-lang.org/packages/debois/elm-mdl/latest/" ]
              [ text "Made By Xoxo Inc." ]
          ]
      ]
  ]


tabs : List ( String, String, Addr -> Model -> Html )
tabs =
  []


tabViews : Array (Addr -> Model -> Html)
tabViews =
  List.map (\( _, _, v ) -> v) tabs |> Array.fromList


tabTitles : List Html
tabTitles =
  List.map (\( x, _, _ ) -> text x) tabs


stylesheet : Html
stylesheet =
  Style.stylesheet ""


view : Signal.Address Action -> Model -> Html
view addr model =
  Layout.view
    (Signal.forwardTo addr LayoutAction)
    model.layout
    { header = header
    , drawer = drawer
    , tabs = []
    , main =
        [ stylesheet ]
        --top
    }
    {- The following line is not needed when you manually set up
    your html, as done with page.html. Removing it will then
    fix the flicker you see on load.
    -}
    |>
      Scheme.topWithScheme Color.Teal Color.Red



-- init : ( Model, Effects.Effects Action )
-- init =
--   ( model, Effects.none )
--
--
-- inputs : List (Signal.Signal Action)
-- inputs =
--   [ Layout.setupSignals LayoutAction ]
--
--
-- app : StartApp.App Model
-- app =
--   StartApp.start
--     { init = init
--     , view = view
--     , update = update
--     , inputs = inputs
--     }
--
--
-- main : Signal Html
-- main =
--   app.html
--
--
--
-- -- PORTS
--
--
-- port tasks : Signal (Task.Task Never ())
-- port tasks =
--   app.tasks
