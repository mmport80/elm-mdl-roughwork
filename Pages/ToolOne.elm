module Pages.ToolOne (..) where

import Html exposing (div, text, Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, on, onWithOptions, defaultOptions)


-- import Html.Attributes exposing (href, class, style)

import Effects exposing (Effects, Never)


-- import Material.Textfield as Textfield
-- import Material.Button as Button

import Material.Scheme
import Material.Helpers as Helpers
import Material
import Dict
import Maybe
import Json.Decode as Json


-- MODEL


type alias Model =
  { mdl : Material.Model
  , partitionAt : Int
  }


largeInt : Int
largeInt =
  6930898827444486145


model : Model
model =
  { mdl = Material.model
  , partitionAt = largeInt
  }



-- ACTION, UPDATE


type Action
  = MDL (Material.Action Action)
  | Partition Int


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case Debug.log "Action" action of
    MDL action' ->
      Material.update MDL action' model.mdl
        |> Helpers.map1st (\mdl' -> { model | mdl = mdl' })

    Partition index ->
      ( { model | partitionAt = index }
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
view address model =
  let
    rs =
      generateData data 5
  in
    div
      [ style [ ( "width", "100%" ), ( "height", "100%" ) ], onClick address (Partition largeInt) ]
      [ div
          []
          (header rs
            ++ (rows address rs
                  |> List.take model.partitionAt
               )
          )
      , div
          []
          (rows address rs
            |> List.drop (model.partitionAt + 1)
          )
      ]
      |> Material.Scheme.top


cellStyle : Html.Attribute
cellStyle =
  style
    [ ( "display", "table-cell" )
    , ( "padding", "1em" )
    ]


table : Signal.Address Action -> List Data -> List Html
table address rs =
  (header rs) ++ (rows address rs)



--generate from a single record


header : List Data -> List Html
header rs =
  case (List.head rs) of
    Just h ->
      [ div
          [ style
              [ ( "display", "table-header-group" )
              , ( "font-weight", "bold" )
              ]
          ]
          --generate columns based on input
          (Dict.keys
            h
            |> List.foldr
                (\c a ->
                  div [ cellStyle ] [ text c ] :: a
                )
                []
          )
      ]

    _ ->
      [ div [] [ text "no data" ] ]


generateData : Data -> Int -> List Data
generateData d rows =
  if rows == 0 then
    []
  else
    d :: generateData d (rows - 1)


type alias Data =
  Dict.Dict String Int


data : Data
data =
  [ ( "a", 1 ), ( "b", 2 ), ( "c", 3 ), ( "d", 4 ), ( "e", 5 ), ( "f", 6 ) ]
    |> Dict.fromList



--generate from records
--click on a row, partition the table into two
--top table has the header
--bottom lacks header


onKlick : Signal.Address Action -> Action -> Html.Attribute
onKlick address action =
  onWithOptions "click" { defaultOptions | stopPropagation = True } Json.value (\_ -> Signal.message address action)


rows : Signal.Address Action -> List Data -> List Html
rows address =
  List.indexedMap
    (\index r ->
      div
        [ style
            [ ( "display", "table-row" )
            , ( "cursor", "pointer" )
            ]
        , onKlick address (Partition index)
        ]
        (Dict.toList r
          |> List.map
              (\( _, c ) ->
                div
                  [ cellStyle ]
                  [ index |> toString >> text ]
              )
        )
    )
