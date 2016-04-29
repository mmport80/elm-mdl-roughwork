module Pages.Login (..) where

import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Effects exposing (Effects, Never)
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Scheme
import Material.Helpers as Helpers
import Material
import Regex exposing (Regex)


-- MODEL


type alias Model =
  { loginButton : Button.Model
  , mdl : Material.Model
  }


model : Model
model =
  { loginButton =
      Button.model True
      -- With ripple animation
  , mdl = Material.model
  }



-- ACTION, UPDATE


type Action
  = MDL (Material.Action Action)
  | Password String
  | Username String
  | LoginButton Button.Action


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    LoginButton action' ->
      let
        ( submodel, fx ) =
          Button.update action' model.loginButton

        model' =
          case action' of
            --do something on click
            --page move is done at router level
            Button.Click ->
              model

            _ ->
              model
      in
        ( { model' | loginButton = submodel }
        , Effects.map LoginButton fx
        )

    MDL action' ->
      Material.update MDL action' model.mdl
        |> Helpers.map1st (\mdl' -> { model | mdl = mdl' })

    Username str ->
      let
        rx =
          ( "Alphanumeric, 2-30 characters", Regex.regex "[a-zA-Z0-9]{2,}" )
      in
        --better to refer to initial model in order to do regex check...
        ( { model | mdl = checkRegex str rx model.mdl username }
        , Effects.none
        )

    Password str ->
      let
        rx =
          ( "Minimum 6 Characters", Regex.regex "(.){6,}" )
      in
        ( { model | mdl = checkRegex str rx model.mdl password }
        , Effects.none
        )



--helpers didn't load for some reason...
-- map1st : (a -> c) -> ( a, b ) -> ( c, b )
-- map1st f ( x, y ) =
--   ( f x, y )
--regex stuff


checkRegex : String -> ( String, Regex.Regex ) -> Material.Model -> Textfield.Instance Material.Model Action -> Material.Model
checkRegex str ( rx', rx ) mdl textField =
  let
    value4 =
      textField.get mdl |> .value
  in
    mdl
      |> textField.map
          (\m ->
            { m
              | error =
                  if match value4 rx then
                    Nothing
                  else
                    rx' |> Just
            }
          )


match : String -> Regex.Regex -> Bool
match str rx =
  Regex.find Regex.All rx str
    |> List.any (.match >> (==) str)



-- VIEW


view : Signal.Address Action -> Model -> Html
view addr model =
  div
    []
    [ div
        []
        [ username.view addr model.mdl [] ]
    , div
        []
        [ span
            []
            [ password.view addr model.mdl [] ]
        , span
            [ style
                [ ( "margin", "auto" )
                , ( "padding-left", "5%" )
                , ( "padding-right", "5%" )
                ]
            ]
            [ Button.flat
                (Signal.forwardTo addr LoginButton)
                model.loginButton
                []
                [ text "Login" ]
            ]
        ]
    ]
    |> Material.Scheme.top



--instantiate textfield model


m0 : Textfield.Model
m0 =
  Textfield.model


username : Textfield.Instance Material.Model Action
username =
  --unique id int identifies components
  Textfield.instance
    0
    MDL
    { m0 | label = Just { text = "Username", float = True } }
    [ Textfield.fwdInput Username ]


password : Textfield.Instance Material.Model Action
password =
  Textfield.instance
    1
    MDL
    { m0 | label = Just { text = "Password", float = True }, kind = Textfield.Password }
    [ Textfield.fwdInput Password ]
