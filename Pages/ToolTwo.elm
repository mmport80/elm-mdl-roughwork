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
          [ ("max-width", "50em")
          , ("min-width", "10em")
          , ("width", "100%")
          , ("margin", "1em auto")
          ]
          ]
      [
          grouping "Today"
        , item
        , item
        , grouping  "This Week"
        , item
        , grouping  "This Week"
        , openGroup
          [ grouping  "Yoyo"
          , item
          , item
          , grouping  "Xoxo"
          , openGroup
            [ div [ style [ ("border-top","1px solid grey"),( "display", "table-row" ),("border-collapse", "collapse"),("padding-top","2em")] ] --style [("display","block")]
              [ span [ style ([("width","25%") ] ++ cellStyle) ] [ text "Details" ]
              , span [ style ([("width","75%") ] ++ cellStyle) ] [ text "" ]
              ]
            , datum
            , datum
            , datum
            , div [ style [ ("border-top","1px solid grey"),( "display", "table-row" ),("padding-top","2em")] ] --style [("display","block")]
              [ span [ style ([("width","25%") ] ++ cellStyle) ] [ text "more..." ]
              , span [ style ([("width","75%") ] ++ cellStyle) ] [ text "" ]
              ]
            --, a [href "ojoj"] [text "more..."]
            ]
          , item
          ]
        , item

        , grouping  "This Month"
        , item
        , item
        , item
        , item
       ]
      |> Material.Scheme.top

--click, then 'drill down'
--more groupings
--more items
--click further to see detail

datum =
  div [ style [ ("border-top","1px solid black"),( "display", "table-row" ),("padding-top","2em")] ] --style [("display","block")]
    [ span [ style ([("width","25%") ] ++ cellStyle) ] [ text "Attribute A" ]
    , span [ style ([("width","75%") ] ++ cellStyle) ] [ text "abababababa" ]
    ]

cellStyle : List (String, String)
cellStyle =
    [ ( "display", "table-cell" )
    , ( "padding", "1em" )
    ]

item : Html
item =
  div []
    [
    Style.div [
      Elevation.e2
      , css "margin" "2em"
      , css "padding" "1em"
      , css "cursor" "pointer"
      ]
      [text "txtxtxtxtxtx"]
    ]

grouping : String -> Html
grouping s = div
  [ style
    [ ("padding","1em")
    , ("display","block")
    ]
  ]
  [ text s ]

--group with list of groups underneath
--open as in a user has clicked to see contents
openGroup : List Html -> Html
openGroup lh = Style.div
  [ Elevation.e2
  , css "display" "block"
  , css "padding" "1em"
  ]
  lh

details : Dict.Dict -> List Html
details d =
  [ grouping "OIOI"
  ]
