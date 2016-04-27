

import Signal exposing (Address, Mailbox)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (..)
import DOM exposing (..)
import String

type alias Model
  = String


model0 : Model
model0 = 
  "(Nothing)"


box : Mailbox Model
box = 
  Signal.mailbox model0


items : Html 
items = 
  [0..5]
  |> List.map (\idx -> 
    li 
      [ class (toString idx) ]
      [ text <| "Item " ++ toString idx ]
  ) -- childNodes
  |> ul [] --childNode 0 (b)


infixr 5 :>
(:>) : (a -> b) -> a -> b
(:>) f x = 
  f x


decode : Decoder String
decode = 
  DOM.target 
  :> parentElement
  :> childNode 0 -- (a)
  :> childNode 0 -- (b)
  :> childNodes className
  |> Decode.map (String.join ", ")


view : Model -> Html
view model = 
  div -- parentElement
    [ class "root" ]
    [ div -- childNode 0 (a)
        [ class "container"]
        [ items ]
    , div 
        [ class "value" ] 
        [ text <| "Model value: " ++ toString model ]
    , button -- target
        [ class "button"
        , on "click" decode  (Signal.message box.address) 
        ]
        [ text "Click" ]
    ]

main : Signal Html
main = 
  Signal.map view box.signal
