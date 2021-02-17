-- Blackout page to draw & hide black boxes on an image

module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser exposing ((<?>), query, s)
import Url.Parser.Query
import Html exposing (Html, Attribute, button, div, text, img, map, h1, h2, a)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (src, style, href)
import Json.Decode as Decode
import Json.Encode as Encode
import Http

-- MAIN

main =
  Browser.application
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- MODEL

type alias Box = 
    { id: Int
    , top: Float
    , left: Float
    , width: Float
    , height: Float
    , shown: Bool
    }

type alias Position =
    { x: Float, y: Float }

type alias Model = 
    { document: String
    , navkey: Nav.Key
    , boxes: List Box
    , selecting: Bool
    , startpos: Position
    , name: String
    , url: String
    }

queryParser : Url.Parser.Parser (Maybe String -> Maybe String) (Maybe String)
queryParser =
    s "index.html" <?> Url.Parser.Query.string "doc"

init : () -> Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  let doc = Maybe.withDefault "0000" ( Maybe.withDefault Nothing (Url.Parser.parse queryParser url))
  in (
  { document = doc
  , navkey = key
  , boxes = []
  , selecting = False
  , startpos = { x = 0.0, y = 0.0 }
  , name = "Loading..."
  , url = ""
  },
  Http.get
    { url = "/document?doc=" ++ doc
    , expect = Http.expectJson GotData documentDecoder
    }
  )


documentDecoder : Decode.Decoder (List Box, String, String)
documentDecoder =
  Decode.map3 (\boxes url name -> (boxes, url, name))
    (Decode.field "boxes" (Decode.list boxDecoder))
    (Decode.field "url" Decode.string)
    (Decode.field "name" Decode.string)

boxDecoder : Decode.Decoder Box
boxDecoder =
  Decode.map6 Box
    (Decode.field "id" Decode.int)
    (Decode.field "top" Decode.float)
    (Decode.field "left" Decode.float)
    (Decode.field "width" Decode.float)
    (Decode.field "height" Decode.float)
    (Decode.succeed True) -- shown


encodeDocument : Model -> Encode.Value
encodeDocument model =
  Encode.object
    [ ( "boxes", Encode.list (\box ->
        Encode.object
          [ ("id", Encode.int box.id),
            ("top", Encode.float box.top),
            ("left", Encode.float box.left),
            ("width", Encode.float box.width),
            ("height", Encode.float box.height)
          ]
      ) model.boxes
      )
    ]


-- UPDATE


type Msg
  = Toggle Int
  | ClickPosition Position
  | GotData (Result Http.Error (List Box, String, String))
  | GotText (Result Http.Error String)
  | Save
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url

min : Float -> Float -> Float
min a b = if a < b then a else b

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Toggle id ->
      ( { model |
          boxes = List.map (\box ->
            if box.id == id then
              { box | shown = not box.shown}
            else box
          ) model.boxes
      }, Cmd.none)

    ClickPosition pos ->
      if not model.selecting then
        ( { model |
            startpos = pos,
            selecting = True }
        , Cmd.none )
      else
        ( { model |
          selecting = False,
          boxes =
            [ { id = List.length model.boxes,
                left = 100 * min pos.x model.startpos.x,
                top  = 100 * min pos.y model.startpos.y,
                width  = 100 * abs(pos.x-model.startpos.x),
                height = 100 * abs(pos.y-model.startpos.y),
                shown = True} ]
            ++ model.boxes
        }, Cmd.none)
    GotText _ ->
      (model, Cmd.none)
    GotData result ->
      case result of
        Ok (boxes,url,name) ->
          ( { model
            | boxes = boxes
            , url = url
            , name = name }
          , Cmd.none )
            --Debug.log fullText (model, Cmd.none)
        Err _ ->
          (model, Cmd.none)
    Save ->
      (model, Http.post
        { url = "/document?doc=" ++ model.document
        , expect = Http.expectString GotText
        , body = Http.jsonBody ( encodeDocument model )
        }
      )

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.load (Url.toString url) )
          --( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( model, Cmd.none )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = ""
  , body = [ div [] [
    div []
    [ h1 [ style "display" "inline-block", style "margin-left" "0.5rem" ] [ text "Blackout" ]
    , h2 [ style "display" "inline-block", style "margin-left" "0.5rem" ] [ text model.name ]
    , a
      [ href "/menu.html"
      , style "margin" "0.5em" ] [ text "Menu" ]
    , button [ onClick Save ] [ text "Save" ]
    ]
    , div [ style "position" "relative" ] [
        img [
          style "width" "100%",
          style "filter" (if model.selecting then "brightness(0.9)" else "unset"),
          src model.url
        ] [ text "+" ],
        blackBoxes model
      ]
    ]
    ]}

blackBoxes : Model -> Html Msg
blackBoxes model = 
  div [] (
      -- A div to capture the non-box click events
      div [ style "position" "absolute"
        , style "width" "100%"
        , style "height" "100%"
        , style "top" "0px"
        , style "left" "0px"
        , style "cursor" "crosshair"
        , onClickPos
        ] []
      :: (
      List.map (\box -> div
        [ style "position" "absolute"
        , style "backgroundColor" ( if box.shown then "black" else "unset" )
        , style "top" ( String.fromFloat ( box.top - 0.5 ) ++ "%")
        , style "left" ( String.fromFloat ( box.left - 0.5 ) ++ "%")
        , style "width" ( String.fromFloat ( box.width + 1 ) ++ "%")
        , style "height" ( String.fromFloat ( box.height + 1 ) ++ "%")
        , style "cursor" "pointer"
        , onClick (Toggle box.id)
        ] []) model.boxes 
    ) )

genPosition : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Msg
genPosition sleft stop left top w h x y =
    ClickPosition ({
        x = toFloat (x-left+sleft)/(toFloat w),
        y = toFloat (y-top +stop )/(toFloat h)
    })

onClickPos : Attribute Msg
onClickPos =
  on "click" (Decode.map8 genPosition
      (Decode.at [ "target", "parentNode", "parentNode", "parentNode", "parentNode", "parentNode", "scrollLeft" ] Decode.int)
      (Decode.at [ "target", "parentNode", "parentNode", "parentNode", "parentNode", "parentNode", "scrollTop" ] Decode.int)
      (Decode.at [ "target", "parentNode", "parentNode", "offsetLeft" ] Decode.int)
      (Decode.at [ "target", "parentNode", "parentNode", "offsetTop" ] Decode.int)
      (Decode.at [ "target", "offsetWidth" ] Decode.int)
      (Decode.at [ "target", "offsetHeight" ] Decode.int)
      (Decode.at [ "clientX" ] Decode.int)
      (Decode.at [ "clientY" ] Decode.int))
