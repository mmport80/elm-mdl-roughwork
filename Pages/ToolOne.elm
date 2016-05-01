module Pages.ToolOne (..) where

import Html exposing (..)


-- import Html.Attributes exposing (href, class, style)

import Effects exposing (Effects, Never)


-- import Material.Textfield as Textfield
-- import Material.Button as Button

import Material.Scheme
import Material.Helpers as Helpers
import Material
import Material.Style as Style


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



--generate data
--two tables
--one above selected
--one below
--top table has headers
--bottom is headerless
-- VIEW


view : Signal.Address Action -> Model -> Html
view addr model =
  div
    []
    [ text "1" ]
    |> Material.Scheme.top


stylesheet : Html
stylesheet =
  Style.stylesheet """
  blockquote:before { content: none; }
  blockquote:after { content: none; }
  blockquote {
    border-left-style: solid;
    border-width: 1px;
    padding-left: 1.3ex;
    border-color: rgb(255,82,82);
    font-style: normal;
      /* TODO: Really need a way to specify "secondary color" in
         inline css.
       */
  }
  p, blockquote {
    max-width: 40em;
  }
  h1, h2 {
    /* TODO. Need typography module with kerning. */
    margin-left: -3px;
  }
"""
