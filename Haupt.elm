import StartApp
import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Effects exposing (Effects, Never)
import Task exposing (Task)

import Material.Button as Button
import Material.Textfield as Textfield
import Material.Scheme
import Material

import Regex

--import Material.Helpers

-- MODEL

type alias Model =
  { count : Int
  , mdl : Material.Model
  , rx : (String, Regex.Regex)
  , login : Button.Model
  }


model : Model
model =
  { count = 0
  , mdl = Material.model
  , rx = ("Numbers Please", Regex.regex "[0-9]*")
  , login = Button.model True -- With ripple animation
  }


-- ACTION, UPDATE


--helpers didn't load for some reason...
map1st : (a -> c) -> (a,b) -> (c,b)
map1st f (x,y) = (f x, y)

--check
setRegex : String -> (String, Regex.Regex)
setRegex str = (str, Regex.regex str)

match : String -> Regex.Regex -> Bool
match str rx =
  Regex.find Regex.All rx str
    |> List.any (.match >> (==) str)

checkRegex : String -> (String, Regex.Regex) -> Material.Model -> Textfield.Instance Material.Model Action -> Material.Model
checkRegex str (rx', rx) mdl textField =
  let
    value4 = textField.get mdl |> .value
  in
    mdl |> textField.map (\m -> { m | error =
      if match value4 rx then
        Nothing
      else
        rx' |> Just
      })


type Action
  = MDL (Material.Action Action)
  | Password String
  | Username String
  | Login Button.Action


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    --updates models in MDL model
    MDL action' ->
      Material.update MDL action' model.mdl
        |> map1st (\mdl' -> { model | mdl = mdl' })
    Login a ->
      let
        (submodel, fx) =
          Button.update a model.login
        model' =
          case a of
            Button.Click ->
              model
            _ ->
              model
      in
        ( { model' | login = submodel }
        , Effects.map Login fx
        )
    Username str ->
      --have to refer to initial model in order to do regex check...
      ( { model | mdl = checkRegex str model.rx model.mdl username }
      , Effects.none
      )
    Password str ->
      ( { model | mdl = checkRegex str model.rx model.mdl password }
      , Effects.none
      )

--instantiate textfield model
m0 : Textfield.Model
m0 = Textfield.model

-- VIEW
username : Textfield.Instance Material.Model Action
username =
  --unique id int identifies components
  Textfield.instance 0 MDL
    { m0 | label = Just {  text = "Username", float = True } }
    [ Textfield.fwdInput Username ]

password : Textfield.Instance Material.Model Action
password =
  Textfield.instance 1 MDL
    { m0 | label = Just {  text = "Password", float = True } }
    [ Textfield.fwdInput Password ]

view : Signal.Address Action -> Model -> Html
view addr model =
  div
    [ style
      [ ("margin", "auto")
      , ("padding-left", "5%")
      , ("padding-right", "5%")
      ]
    ]
    [ div [] [ username.view addr model.mdl [] ]
    , div []
      [ password.view addr model.mdl []
      --on button press go to new screen
      --use hop / router
      , Button.flat (Signal.forwardTo addr Login) model.login [] [ text "Login" ]
      ]
    ]
  --css "scheme" loader
  |> Material.Scheme.top


{- The remainder of this file is Elm/StartApp boilerplate.
-}


-- SETUP


init : (Model, Effects.Effects Action)
init = (model, Effects.none)


inputs : List (Signal.Signal Action)
inputs = []


app : StartApp.App Model
app = StartApp.start
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
