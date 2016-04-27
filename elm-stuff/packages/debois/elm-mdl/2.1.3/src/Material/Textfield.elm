module Material.Textfield where

{-| From the [Material Design Lite documentation](http://www.getmdl.io/components/#textfields-section):

> The Material Design Lite (MDL) text field component is an enhanced version of
> the standard HTML `<input type="text">` and `<input type="textarea">` elements.
> A text field consists of a horizontal line indicating where keyboard input
> can occur and, typically, text that clearly communicates the intended
> contents of the text field. The MDL text field component provides various
> types of text fields, and allows you to add both display and click effects.
>
> Text fields are a common feature of most user interfaces, regardless of a
> site's content or function. Their design and use is therefore an important
> factor in the overall user experience. See the text field component's
> [Material  Design specifications page](https://www.google.com/design/spec/components/text-fields.html)
> for details.
>
> The enhanced text field component has a more vivid visual look than a standard
> text field, and may be initially or programmatically disabled. There are three
> main types of text fields in the text field component, each with its own basic
> coding requirements. The types are single-line, multi-line, and expandable.


Refer to 
[this site](https://debois.github.io/elm-mdl/#/textfields)
for a live demo.
 
This implementation provides only single-line.


# Configuration
@docs Kind, Label

# Elm Architecture
@docs Action, Model, model, update, view

# Component
@docs Container, Instance
@docs instance, fwdInput, fwdBlur, fwdFocus

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects

import Material.Helpers exposing (filter)
import Material.Style as Style exposing (cs, cs', Style)
import Material.Component as Component exposing (Indexed)


-- MODEL


{-| Label configuration. The `text` is the text of the label;
the label floats if `float` is True.
-}
type alias Label =
  { text : String
  , float : Bool
  }


{-| Kind of textfield. Currently supports only single-line inputs.
-}
type Kind
  = SingleLine
  {-
  | MultiLine (Maybe Int) -- Max no. of rows or no limit
  -- TODO. Should prevent key event for ENTER
  -- when number of rows exceeds maxrows argument to constructor:

  MaterialTextfield.prototype.onKeyDown_ = function(event) {
    var currentRowCount = event.target.value.split('\n').length;
    if (event.keyCode === 13) {
      if (currentRowCount >= this.maxRows) {
        event.preventDefault();
      }
    }
  };
  -}


{-| Model. The textfield is in its error-container if `error` is not `Nothing`.
The contents of the field is `value`.
-}
type alias Model =
  { label : Maybe Label
  , error : Maybe String
  , kind : Kind
  , isDisabled : Bool
  , isFocused : Bool
  , value : String
  }


{-| Default model. No label, error, or value.
-}
model : Model
model =
  { label = Nothing
  , error = Nothing
  , kind = SingleLine
  , isDisabled = False
  , isFocused = False
  , value = ""
  }


-- ACTIONS, UPDATE


{-| Component actions. `Input` carries the new value of the field.
-}
type Action
  = Input String
  | Blur
  | Focus


{-| Component update.
-}
update : Action -> Model -> Model
update action model =
  case action of
    Input str ->
      { model | value = str }

    Blur ->
      { model | isFocused = False }

    Focus ->
      { model | isFocused = True }


-- VIEW


{-| Component view.
-}
view : Signal.Address Action -> Model -> List Style -> Html
view addr model styles =
  let hasFloat = model.label |> Maybe.map .float |> Maybe.withDefault False
      hasError = model.error |> Maybe.map (always True) |> Maybe.withDefault False
      labelText = model.label |> Maybe.map .text 
  in
    filter Style.div
      (  cs "mdl-textfield"
      :: cs "mdl-js-textfield"
      :: cs "is-upgraded"
      :: cs' "mdl-textfield--floating-label" hasFloat
      :: cs' "is-invalid" hasError
      :: cs' "is-dirty" (model.value /= "")
      :: cs' "is-focused" (model.isFocused && not model.isDisabled)
      :: cs' "is-disabled" model.isDisabled
      :: styles
      )
      [ Just <| input
          [ class "mdl-textfield__input"
          , style [ ("outline", "none") ]
          , type' "text"
          , disabled model.isDisabled
          , value model.value
          , Html.Events.on "input" targetValue (\s -> Signal.message addr (Input s))
          , onBlur addr Blur
          , onFocus addr Focus
          ]
          []
      , Just <| label 
          [class "mdl-textfield__label"]  
          (case labelText of 
            Just str -> [ text str ]
            Nothing -> [])
      , model.error |> Maybe.map (\e ->
          span [class "mdl-textfield__error"] [text e])
      ]



-- COMPONENT 


{-|
-}
type alias Container c = 
  { c | textfield : Indexed Model }


{-| 
-}
type alias Instance container obs = 
  Component.Instance Model container Action obs (List Style -> Html)


{-| Component constructor. See module `Material`.
-}
instance : 
  Int
  -> (Component.Action (Container c) obs -> obs)
  -> Model 
  -> List (Component.Observer Action obs)
  -> Instance (Container c) obs

instance = 
  let 
    update' action model = (update action model, Effects.none)
  in 
    Component.instance view update' .textfield (\x y -> {y | textfield = x}) 


{-| Lift the button Click action to your own action. E.g., 
-}
fwdInput : (String -> obs) -> Action -> Maybe obs
fwdInput f action =
  case action of 
    Input str -> Just (f str)
    _ -> Nothing


{-| Lift the Blur action to your own action. 
-}
fwdBlur : obs -> Action -> Maybe obs
fwdBlur o action = 
  case action of 
    Blur -> Just o
    _ -> Nothing


{-| Lift the Focus action to your own action.
-}
fwdFocus : obs -> Action -> Maybe obs
fwdFocus o action =
  case action of 
    Focus -> Just o 
    _ -> Nothing 

