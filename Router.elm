module Router (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (class)
import Hop
import Hop.Matchers exposing (..)
import Hop.Navigate exposing (navigateTo, setQuery)
import Hop.Types exposing (Config, Query, Location, PathMatcher, Router, newLocation)
import Material.Button as Button
import Pages.Login exposing (..)
import Layout exposing (..)


-- MODEL


type alias Model =
  { location : Location
  , route : Route
  , loginPage : Pages.Login.Model
  , layoutPage : Layout.Model
  }


newModel : Model
newModel =
  { location = newLocation
  , route = LoginRoute
  , loginPage = Pages.Login.model
  , layoutPage = Layout.model
  }



-- ROUTES


type Route
  = NotFoundRoute
  | LoginRoute
  | LayoutRoute


matchers : List (PathMatcher Route)
matchers =
  [ match1 LoginRoute ""
  , match1 LayoutRoute "/layout"
  ]


routerConfig : Config Route
routerConfig =
  { hash = True
  , basePath = ""
  , matchers = matchers
  , notFound = NotFoundRoute
  }


router : Router Route
router =
  Hop.new routerConfig



-- ACTIONS


type Action
  = HopAction ()
  | ApplyRoute ( Route, Location )
  | NavigateTo String
  | SetQuery Query
  | LoginPage Pages.Login.Action
  | Layout Layout.Action


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NavigateTo path ->
      ( model, Effects.map HopAction (navigateTo routerConfig path) )

    SetQuery query ->
      ( model, Effects.map HopAction (setQuery routerConfig query model.location) )

    ApplyRoute ( route, location ) ->
      ( { model | route = route, location = location }, Effects.none )

    HopAction () ->
      ( model, Effects.none )

    LoginPage input ->
      --case button click, nav to
      let
        ( login, fx ) =
          Pages.Login.update input model.loginPage

        model' =
          { model | loginPage = login }

        default =
          ( model', Effects.map LoginPage fx )
      in
        case input of
          LoginButton a ->
            case a of
              Button.Click ->
                ( model'
                , Effects.batch
                    [ Effects.map HopAction (navigateTo routerConfig "layout")
                    , Effects.map LoginPage fx
                    ]
                )

              _ ->
                default

          _ ->
            default

    Layout input ->
      let
        ( layoutPage, fx ) =
          Layout.update input model.layoutPage

        model' =
          { model | layoutPage = layoutPage }
      in
        ( model', Effects.map Layout fx )



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ pageView address model
    ]


currentQuery : Model -> Html
currentQuery model =
  let
    query =
      toString model.location.query
  in
    span
      [ class "labelQuery" ]
      [ text query ]


pageView : Signal.Address Action -> Model -> Html
pageView address model =
  case model.route of
    NotFoundRoute ->
      div [] [ h2 [ class "title" ] [ text "Not found" ] ]

    LoginRoute ->
      div [] [ Pages.Login.view (Signal.forwardTo address LoginPage) model.loginPage ]

    LayoutRoute ->
      div [] [ Layout.view (Signal.forwardTo address Layout) model.layoutPage ]
