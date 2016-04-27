import StartApp
import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Effects exposing (Effects, Never)
import Task exposing (Task)

import Material.Button as Button
import Material.Textfield as Textfield
import Material.Scheme
import Material

--import Material.Helpers

-- MODEL

type alias Model =
  { count : Int
  , increaseButtonModel : Button.Model
  , resetButtonModel : Button.Model
  , mdl : Material.Model
  }


model : Model
model =
  { count = 0
  , increaseButtonModel = Button.model True -- With ripple animation
  , resetButtonModel = Button.model False   -- Without ripple animation
  , mdl = Material.model
  }


-- ACTION, UPDATE


type Action
  = IncreaseButtonAction Button.Action
  | ResetButtonAction Button.Action
  | MDL (Material.Action Action)
  | Upd0 String


increase : Model -> Model
increase model =
  { model | count = model.count + 1 }


reset : Model -> Model
reset model =
   { model | count = 0 }


map1st : (a -> c) -> (a,b) -> (c,b)
map1st f (x,y) = (f x, y)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    MDL action' ->
      Material.update MDL action' model.mdl
        |> map1st (\mdl' -> { model | mdl = mdl' })
    IncreaseButtonAction action' ->
      let
        (submodel, fx) =
          Button.update action' model.increaseButtonModel
        model' =
          case action' of
            Button.Click ->
              increase model
            _ ->
              model
      in
        ( { model' | increaseButtonModel = submodel }
        , Effects.map IncreaseButtonAction fx
        )
    ResetButtonAction action' ->
      let
        (submodel, fx) =
          Button.update action' model.resetButtonModel
        model' =
          case action' of
            Button.Click ->
              reset model
            _ ->
              model
      in
        ( { model' | resetButtonModel = submodel }
        , Effects.map ResetButtonAction fx
        )
    Upd0 str ->
      let
        model' = { model | mdl = transferToDisabled str model.mdl }
      in
        ( model' , Effects.none )


-- VIEW
transferToDisabled : String -> Material.Model -> Material.Model
transferToDisabled str = field0.map (\m -> { m | value = str } )

field0 : Textfield.Instance Material.Model Action
field0 = Textfield.instance 0 MDL Textfield.model [ Textfield.fwdInput Upd0 ]

view : Signal.Address Action -> Model -> Html
view addr model =
  div
    [ style
      [ ("margin", "auto")
      , ("padding-left", "5%")
      , ("padding-right", "5%")
      ]
    ]
    [ text ("Current count: " ++ toString model.count )
    , Button.flat
        (Signal.forwardTo addr IncreaseButtonAction)
        model.increaseButtonModel
        []
        [ text "Increase" ]
    , Button.flat
        (Signal.forwardTo addr ResetButtonAction)
        model.resetButtonModel
        []
        [ text "Reset" ]
    , field0.view addr model.mdl []

    ]
  |> Material.Scheme.top


{- The remainder of this file is Elm/StartApp boilerplate.
-}


-- SETUP


init : (Model, Effects.Effects Action)
init = (model, Effects.none)


inputs : List (Signal.Signal Action)
inputs =
  [
  ]


app : StartApp.App Model
app =
    StartApp.start
      { init = init
      , view = view
      , update = update
      , inputs = inputs
      }


main : Signal Html
main =
    app.html


-- PORTS


port tasks : Signal (Task Never ())
port tasks =
    app.tasks
