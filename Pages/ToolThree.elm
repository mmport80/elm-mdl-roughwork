module Pages.ToolThree (..) where

import Html exposing (..)


-- import Html.Attributes exposing (href, class, style)

import Effects exposing (Effects, Never)


-- import Material.Textfield as Textfield
-- import Material.Button as Button

import Material.Scheme
import Material.Helpers as Helpers
import Material


-- MODEL


type alias Model =
  { mdl : Material.Model
  }


model : Model
model =
  { mdl = Material.model
  }



-- ACTION, UPDATE


type Action
  = MDL (Material.Action Action)
  | XOXO String


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    MDL action' ->
      Material.update MDL action' model.mdl
        |> Helpers.map1st (\mdl' -> { model | mdl = mdl' })

    _ ->
      ( model
      , Effects.none
      )



--helpers didn't load for some reason...
-- map1st : (a -> c) -> ( a, b ) -> ( c, b )
-- map1st f ( x, y ) =
--   ( f x, y )
--regex stuff
-- VIEW


view : Signal.Address Action -> Model -> Html
view addr model =
  div
    []
    [ text "3" ]
    |> Material.Scheme.top
