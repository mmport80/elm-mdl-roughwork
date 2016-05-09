module Pages.ToolThree (..) where

import Html exposing (div, text, Html, hr, a, span)
import Html.Attributes exposing (style, href)


--import Html.Events exposing (onClick, on, onWithOptions, defaultOptions)
-- import Html.Attributes exposing (href, class, style)

import Effects exposing (Effects, Never)


-- import Material.Textfield as Textfield
-- import Material.Button as Button

import Material.Scheme
import Material.Helpers as Helpers
import Material.Elevation as Elevation
import Material.Style as Style exposing (Style, css)


--
--

import Dict


-- import Maybe
-- import Json.Decode as Json

import Material


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
  (div
    [ style
        [ ( "max-width", "50em" )
        , ( "min-width", "10em" )
        , ( "width", "100%" )
        , ( "margin", "1em auto" )
        ]
    ]
    ([])
  )
    |> Material.Scheme.top


datum : String -> String -> Html
datum k v =
  div
    [ style (rowStyle ++ lineStyle) ]
    [ span [ style (leftCellStyle ++ cellStyle) ] [ text k ]
    , span [ style (rightCellStyle ++ cellStyle) ] [ text v ]
    ]


rowStyle : List ( String, String )
rowStyle =
  [ ( "display", "table-row" ), ( "padding-top", "2em" ) ]


lineStyle : List ( String, String )
lineStyle =
  [ ( "border-top", "thin solid lightgray" ) ]


cellStyle : List ( String, String )
cellStyle =
  [ ( "display", "table-cell" )
  , ( "padding", "1em" )
  ]


rightCellStyle : List ( String, String )
rightCellStyle =
  [ ( "width", "75%" ) ]


leftCellStyle : List ( String, String )
leftCellStyle =
  [ ( "width", "25%" ) ]


item : String -> Html
item s =
  div
    []
    [ Style.div
        [ Elevation.e2
        , css "margin" "2em"
        , css "padding" "1em"
        , css "cursor" "pointer"
        ]
        [ text s ]
    ]


grouping : String -> Html
grouping s =
  div
    [ style
        [ ( "padding", "1em" )
        , ( "display", "block" )
        ]
    ]
    [ text s ]



--group with list of groups underneath
--open as in a user has clicked to see contents


openGroup : List Html -> Html
openGroup lh =
  Style.div
    [ Elevation.e2
    , css "display" "block"
    , css "padding" "1em"
    ]
    lh


details : Dict.Dict -> List Html
details d =
  [ grouping "OIOI"
  ]


data : List (List ( String, String ))
data =
  [ [ ( "Date", "Today" ), ( "Region", "North" ), ( "Colour", "Purple" ), ( "Attribute A", "43" ), ( "Attribute B", "74" ), ( "Attribute C", "27" ) ]
  , [ ( "Date", "Today" ), ( "Region", "South" ), ( "Colour", "Green" ), ( "Attribute A", "16" ), ( "Attribute B", "19" ), ( "Attribute C", "16" ) ]
  , [ ( "Date", "This Week" ), ( "Region", "East" ), ( "Colour", "Yellow" ), ( "Attribute A", "74" ), ( "Attribute B", "62" ), ( "Attribute C", "46" ) ]
  , [ ( "Date", "This Week" ), ( "Region", "North" ), ( "Colour", "Red" ), ( "Attribute A", "9" ), ( "Attribute B", "84" ), ( "Attribute C", "35" ) ]
  , [ ( "Date", "This Week" ), ( "Region", "North" ), ( "Colour", "Blue" ), ( "Attribute A", "69" ), ( "Attribute B", "5" ), ( "Attribute C", "92" ) ]
  , [ ( "Date", "This Week" ), ( "Region", "North" ), ( "Colour", "Purple" ), ( "Attribute A", "73" ), ( "Attribute B", "94" ), ( "Attribute C", "46" ) ]
  , [ ( "Date", "This Week" ), ( "Region", "West" ), ( "Colour", "Green" ), ( "Attribute A", "101010101010" ), ( "Attribute B", "42" ), ( "Attribute C", "112" ) ]
  , [ ( "Date", "This Month" ), ( "Region", "South" ), ( "Colour", "Yellow" ), ( "Attribute A", "12" ), ( "Attribute B", "52" ), ( "Attribute C", "15" ) ]
  , [ ( "Date", "This Month" ), ( "Region", "East" ), ( "Colour", "Red" ), ( "Attribute A", "6" ), ( "Attribute B", "57" ), ( "Attribute C", "4" ) ]
  , [ ( "Date", "This Month" ), ( "Region", "North" ), ( "Colour", "Blue" ), ( "Attribute A", "93" ), ( "Attribute B", "13" ), ( "Attribute C", "86" ) ]
  , [ ( "Date", "This Month" ), ( "Region", "West" ), ( "Colour", "Purple" ), ( "Attribute A", "24" ), ( "Attribute B", "76" ), ( "Attribute C", "35" ) ]
  ]


type alias Datum =
  Dict.Dict String String


type alias Data =
  List Datum


type alias GroupedData =
  Dict.Dict String Data


data2Dict : Data
data2Dict =
  List.map Dict.fromList data

-- groupBy : List String -> Data -> Maybe (Dict.Dict String (Maybe GroupedData))
-- groupBy ss ds =
--   case List.head ss of
--     Just h ->
--       case (groupBy' h ds) of
--         Just r ->
--           Dict.map (\k v -> groupBy' h v) r |> Just
--         Nothing ->
--           Nothing
--
--     --take list from this result
--     --map with groupby
--     --no more groupby strings, return ds straight
--     Nothing ->
--       Nothing


groupBy : List String -> Data -> Maybe GroupedData
groupBy ss ds =
  case ss of
    [] ->
      Nothing
    h :: [] ->
      groupBy' h ds
    h :: t ->
      case groupBy' h ds of
        Just r ->
          Dict.map (\k v -> v) r |> Just
        Nothing ->
          Nothing


groupBy' : String -> Data -> Maybe GroupedData
groupBy' s ds =
  let
    result =
      List.foldl
        (\d a ->
          --look for value of key 's' in currently selected dictionary
          case Dict.get s d of
            Just k ->
              case Dict.get k a of
                --if key already exists, append to preexisting dict
                Just b ->
                  Dict.insert k (d :: b) a

                --create and insert new dictionary
                Nothing ->
                  Dict.insert k [ d ] a

            --if not, skip
            Nothing ->
              a
        )
        Dict.empty
        ds
  in
    --if no grouping was not possible, return nothing
    if result == Dict.empty then
      Nothing
    else
      Just result
