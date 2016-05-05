module Router (..) where

import Effects exposing (Effects, Never)
import Html exposing (div, Html, h2, text)
import Html.Attributes exposing (class)
import Hop
import Hop.Matchers exposing (match1)
import Hop.Navigate exposing (navigateTo, setQuery)
import Hop.Types exposing (Config, Query, Location, PathMatcher, Router, newLocation)
import Material.Button as Button
import Pages.Login exposing (update, Action)
import Pages.Layout exposing (Model, model, Page)


-- MODEL


type alias Model =
  { location : Location
  , route : Route
  , loginPage : Pages.Login.Model
  , layoutPage : Pages.Layout.Model
  }


model : Model
model =
  { location = newLocation
  , route = LoginRoute
  , loginPage = Pages.Login.model
  , layoutPage = Pages.Layout.model
  }



-- ROUTES


type Route
  = NotFoundRoute
  | LoginRoute
  | LayoutRoute
  | ToolOneRoute
  | ToolTwoRoute
  | ToolThreeRoute


matchers : List (PathMatcher Route)
matchers =
  [ match1 LoginRoute ""
  , match1 LayoutRoute "/layout"
  , match1 ToolOneRoute "/toolone"
  , match1 ToolTwoRoute "/tooltwo"
  , match1 ToolThreeRoute "/toolthree"
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
  | Layout Pages.Layout.Action


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NavigateTo path ->
      ( model, Effects.map HopAction (navigateTo routerConfig path) )

    SetQuery query ->
      ( model, Effects.map HopAction (setQuery routerConfig query model.location) )

    ApplyRoute ( route, location ) ->
      --only works when press enter on route, then sticky until next time
      --perhaps move to navigateto?
      --or hop action?!?!
      let
        layoutPage =
          model.layoutPage

        --match routes to page types from layout module
        layoutPage' =
          case route of
            ToolTwoRoute ->
              { layoutPage | currentPage = Pages.Layout.Two }
            ToolThreeRoute ->
              { layoutPage | currentPage = Pages.Layout.Three }
            _ ->
              { layoutPage | currentPage = Pages.Layout.One }
      in
        --send through route?
        --import page options from layout?
        --link routes with pages?
        ( { model
            | route = route
            , location = location
            , layoutPage = layoutPage'
          }
        , Effects.none
        )

    HopAction () ->
      ( model, Effects.none )

    Layout input ->
      let
        ( layoutPage, fx ) =
          Pages.Layout.update input model.layoutPage

        model' =
          { model | layoutPage = layoutPage }
      in
        ( model', Effects.map Layout fx )

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
          Pages.Login.LoginButton a ->
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



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ pageView address model
    ]


pageView : Signal.Address Action -> Model -> Html
pageView address model =
  case model.route of
    NotFoundRoute ->
      div [] [ h2 [ class "title" ] [ text "Not found" ] ]

    LoginRoute ->
      div [] [ Pages.Login.view (Signal.forwardTo address LoginPage) model.loginPage ]

    _ ->
      div [] [ Pages.Layout.view (Signal.forwardTo address Layout) model.layoutPage ]
