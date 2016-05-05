module Pages.Layout (..) where

-- import StartApp

import Html exposing (Html, div, text)
import Html.Attributes exposing (href, class, style, key)
import Signal exposing (Signal)
import Effects exposing (none, Effects)
import Signal


-- import Task exposing (Task)

import Material.Color as Color
import Material.Layout
import Material.Layout as Layout exposing (defaultLayoutModel, Mode)
import Material.Helpers exposing (lift, lift')
import Material.Scheme as Scheme
import Pages.ToolOne exposing (..)
import Pages.ToolTwo exposing (..)
import Pages.ToolThree exposing (..)

import Debug

-- MODEL


layoutModel : Layout.Model
layoutModel =
  defaultLayoutModel


type alias Model =
  { layout : Layout.Model
  , toolOnePage :
      Pages.ToolOne.Model
  , toolTwoPage :
      Pages.ToolTwo.Model
  , toolThreePage :
      Pages.ToolThree.Model
  , currentPage :
      Page
  }


type Page
  = One
  | Two
  | Three


model : Model
model =
  { layout = { layoutModel | mode = Material.Layout.Scroll }
  , toolOnePage =
      Pages.ToolOne.model
  , toolTwoPage =
      Pages.ToolTwo.model
  , toolThreePage =
      Pages.ToolThree.model
      --is a page action
  , currentPage = One
  }



-- ACTION, UPDATE


type Action
  = LayoutAction Layout.Action
  | ToolOnePage Pages.ToolOne.Action
  | ToolTwoPage Pages.ToolTwo.Action
  | ToolThreePage Pages.ToolThree.Action


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case Debug.log "Layout action " action of
    LayoutAction a ->
      let
        ( lifted, layoutFx ) =
          lift .layout (\m x -> { m | layout = x }) LayoutAction Layout.update a model

      in
        ( lifted
        , Effects.batch [ layoutFx ] )

    --one action per page
    --each page maps to a component
    ToolOnePage input ->
      let
        ( toolOnePage, fx ) =
          Pages.ToolOne.update input model.toolOnePage

        model' =
          { model | toolOnePage = toolOnePage }
      in
        ( model', Effects.none )

    ToolTwoPage input ->
      let
        ( toolTwoPage, fx ) =
          Pages.ToolTwo.update input model.toolTwoPage

        model' =
          { model | toolTwoPage = toolTwoPage }
      in
        ( model', Effects.none )

    ToolThreePage input ->
      let
        ( toolThreePage, fx ) =
          Pages.ToolThree.update input model.toolThreePage

        model' =
          { model | toolThreePage = toolThreePage }
      in
        ( model', Effects.none )



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  let
    h = case model.currentPage of
        Two ->
          "Tool Two"
        Three ->
          "Tool Three"
        _ ->
          "Tool One"
    t =
      case model.currentPage of
        One ->
          div [] [ Pages.ToolOne.view (Signal.forwardTo address ToolOnePage) model.toolOnePage ]

        --default
        Two ->
          div [] [ Pages.ToolTwo.view (Signal.forwardTo address ToolTwoPage) model.toolTwoPage ]

        _ ->
          div [] [ Pages.ToolThree.view (Signal.forwardTo address ToolThreePage) model.toolThreePage ]
  in
    Layout.view
      (Signal.forwardTo address LayoutAction)
      model.layout
      { header = header h
      , drawer = drawer
      , tabs = []
      , main =
          [ t ]
      }
      |> Scheme.topWithScheme Color.Teal Color.Red


drawer : List Html
drawer =
  [ Layout.title "Menu"
  , Layout.navigation
      [ Layout.link
          [ href "Main.elm#/toolone" ]
          [ text "Tool 1" ]
      , Layout.link
          [ href "Main.elm#/tooltwo" ]
          [ text "Tool 2" ]
      , Layout.link
          [ href "Main.elm#/toolthree" ]
          [ text "Tool 3" ]
      ]
  ]


header : String -> List Html
header s =
  [ Layout.row
      [ Layout.title s
      , Layout.spacer
      , Layout.navigation
          [ Layout.link
              [ href "http://package.elm-lang.org/packages/debois/elm-mdl/latest/" ]
              [ text "Made By Xoxo Inc." ]
          ]
      ]
  ]
