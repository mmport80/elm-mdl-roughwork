module Pages.ToolTwo (..) where

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
  div
    [ style
        [ ( "max-width", "50em" )
        , ( "min-width", "10em" )
        , ( "width", "100%" )
        , ( "margin", "1em auto" )
        ]
    ]
    [ grouping "Today"
    , item "North"
    , item "South"
    , grouping "This Week"
    , item "East"
    , openGroup
        [ grouping "North"
        , item "Red"
        , item "Blue"
        , openGroup
            [ grouping "Green"
            , div
                [ style [ ( "display", "table" ), ( "border-collapse", "collapse" ), ( "width", "100%" ) ] ]
                [ div
                    [ style rowStyle ]
                    [ span [ style cellStyle ] [ text "" ]
                    , span [ style cellStyle ] [ text "" ]
                    ]
                , div
                    [ style rowStyle ]
                    --style [("display","block")]
                    [ span [ style cellStyle ] [ text "Attribute A" ]
                    , span [ style cellStyle ] [ text "10101010101010" ]
                    ]
                , datum "Attribute B" "42"
                , datum "Attribute C" "112"
                , div
                    [ style rowStyle ]
                    [ span [ style cellStyle ] [ text "" ]
                    , span [ style cellStyle ] [ text "" ]
                    ]
                  -- more
                , div
                    [ style rowStyle ]
                    --style [("display","block")]
                    [ span [ style cellStyle ] [ a [ href "xoxo" ] [ text "More..." ] ]
                    , span [ style cellStyle ] [ text "" ]
                    ]
                ]
              --, a [href "ojoj"] [text "more..."]
            ]
        , item "Purple"
        ]
    , item "West"
    , grouping "This Month"
    , item "North"
    , item "East"
    , item "West"
    , item "South"
    ]
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
