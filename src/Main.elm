-- Blackout page to draw & hide black boxes on an image

module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, button, div, text, img, map, h1)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (src, style)
import Json.Decode as Decode
import Http

-- MAIN

main =
  Browser.element {
    init = init,
    subscriptions = subscriptions,
    update = update,
    view = view }

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
    { page: Int
    , boxes: List Box
    , selecting: Bool
    , startpos: Position
    }


init : () -> (Model, Cmd Msg)
init _ = (
  { page = 0
  , boxes = []
  , selecting = False
  , startpos = { x = 0.0, y = 0.0 }
  },
  Http.get
    { url = "http://localhost:8090/document?doc=0000"
    , expect = Http.expectJson GotData documentDecoder
    }
  )


documentDecoder : Decode.Decoder (List Box)
documentDecoder =
  Decode.field "boxes" (Decode.list boxDecoder)

boxDecoder : Decode.Decoder Box
boxDecoder =
  Decode.map6 Box
    (Decode.field "id" Decode.int)
    (Decode.field "top" Decode.float)
    (Decode.field "left" Decode.float)
    (Decode.field "height" Decode.float)
    (Decode.field "width" Decode.float)
    (Decode.succeed True) -- shown


-- UPDATE


type Msg
  = Increment
  | Decrement
  | Toggle Int
  | ClickPosition Position
  | GotData (Result Http.Error (List Box))

min : Float -> Float -> Float
min a b = if a < b then a else b

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increment ->
      ( { model | page = model.page + 1 }
      , Cmd.none )

    Decrement ->
      ( { model | page = model.page + 1 }
      , Cmd.none )

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
    GotData result ->
      case result of
        Ok boxes ->
          ( { model |
              boxes = boxes }
          , Cmd.none )
            --Debug.log fullText (model, Cmd.none)
        Err _ ->
          (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Blackout" ]
    , button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model.page) ]
    , button [ onClick Increment ] [ text "+" ]
    , div [ style "position" "relative" ] [
        img [
          style "width" "100%",
          style "filter" (if model.selecting then "brightness(0.9)" else "unset"),
          src "material/20210214_111358860.jpg"
        ] [ text "+" ],
        blackBoxes model
      ]
    ]

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
