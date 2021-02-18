-- Blackout page to draw & hide black boxes on an image


module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Attribute, Html, a, br, button, div, h1, h2, img, map, text)
import Html.Attributes exposing (href, src, style)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url exposing (Url)
import Url.Parser exposing ((<?>), query, s)
import Url.Parser.Query



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
    { id : Int
    , top : Float
    , left : Float
    , width : Float
    , height : Float
    , shown : Bool
    }


type alias Position =
    { x : Float, y : Float }


type Mode
    = Viewing
    | Editing
    | Deleting


type alias DocumentInfo =
    { boxes : List Box
    , name : String
    , prev : String
    , next : String
    , url : String
    }


type alias EditingStatus =
    { selecting : Bool
    , startpos : Position
    , mode : Mode
    , newboxids : List Int
    }


type alias Model =
    { document : String
    , navkey : Nav.Key
    , documentinfo : DocumentInfo
    , editingstatus : EditingStatus
    }


queryParser : Url.Parser.Parser (Maybe String -> Maybe String) (Maybe String)
queryParser =
    s "index.html" <?> Url.Parser.Query.string "doc"


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        doc =
            Maybe.withDefault "0000" (Maybe.withDefault Nothing (Url.Parser.parse queryParser url))
    in
    ( { document = doc
      , navkey = key
      , editingstatus =
            { selecting = False
            , startpos = { x = 0.0, y = 0.0 }
            , mode = Viewing
            , newboxids = []
            }
      , documentinfo =
            { name = "Loading..."
            , boxes = []
            , prev = ""
            , next = ""
            , url = ""
            }
      }
    , Http.get
        { url = "/document?doc=" ++ doc
        , expect = Http.expectJson GotData documentDecoder
        }
    )


documentDecoder : Decode.Decoder DocumentInfo
documentDecoder =
    Decode.map5 DocumentInfo
        (Decode.field "boxes" (Decode.list boxDecoder))
        (Decode.field "name" Decode.string)
        (Decode.field "prev" Decode.string)
        (Decode.field "next" Decode.string)
        (Decode.field "url" Decode.string)


boxDecoder : Decode.Decoder Box
boxDecoder =
    Decode.map6 Box
        (Decode.field "id" Decode.int)
        (Decode.field "top" Decode.float)
        (Decode.field "left" Decode.float)
        (Decode.field "width" Decode.float)
        (Decode.field "height" Decode.float)
        (Decode.succeed True)



-- shown


encodeDocument : Model -> Encode.Value
encodeDocument model =
    Encode.object
        [ ( "boxes"
          , Encode.list
                (\box ->
                    Encode.object
                        [ ( "id", Encode.int box.id )
                        , ( "top", Encode.float box.top )
                        , ( "left", Encode.float box.left )
                        , ( "width", Encode.float box.width )
                        , ( "height", Encode.float box.height )
                        ]
                )
                model.documentinfo.boxes
          )
        ]



-- UPDATE


type Msg
    = Toggle Int
    | ClickPosition Position
    | GotData (Result Http.Error DocumentInfo)
    | GotText (Result Http.Error String)
    | Save
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | SetModeViewing
    | SetModeEditing
    | UndoLastBox


min : Float -> Float -> Float
min a b =
    if a < b then
        a

    else
        b


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle id ->
            let
                documentinfo =
                    model.documentinfo
            in
            ( { model
                | documentinfo =
                    { documentinfo
                        | boxes =
                            List.map
                                (\box ->
                                    if box.id == id then
                                        { box | shown = not box.shown }

                                    else
                                        box
                                )
                                documentinfo.boxes
                    }
              }
            , Cmd.none
            )

        UndoLastBox ->
            let
                documentinfo =
                    model.documentinfo

                lastboxid =
                    Maybe.withDefault -1
                        (List.head model.editingstatus.newboxids)
            in
            ( { model
                | documentinfo =
                    { documentinfo
                        | boxes =
                            List.filter
                                (\box ->
                                    box.id /= lastboxid
                                )
                                documentinfo.boxes
                    }
              }
            , Cmd.none
            )

        ClickPosition pos ->
            let
                editingstatus =
                    model.editingstatus
            in
            if not model.editingstatus.selecting then
                ( { model
                    | editingstatus =
                        { editingstatus
                            | startpos = pos
                            , selecting = True
                        }
                  }
                , Cmd.none
                )

            else
                let
                    documentinfo =
                        model.documentinfo

                    newboxid =
                        Maybe.withDefault -1
                            (List.maximum
                                (List.map (\box -> box.id) model.documentinfo.boxes)
                            )
                            + 1
                in
                ( { model
                    | editingstatus =
                        { editingstatus
                            | selecting = False
                            , newboxids = newboxid :: editingstatus.newboxids
                        }
                    , documentinfo =
                        { documentinfo
                            | boxes =
                                [ { id = newboxid
                                  , left = 100 * min pos.x model.editingstatus.startpos.x
                                  , top = 100 * min pos.y model.editingstatus.startpos.y
                                  , width = 100 * abs (pos.x - model.editingstatus.startpos.x)
                                  , height = 100 * abs (pos.y - model.editingstatus.startpos.y)
                                  , shown = True
                                  }
                                ]
                                    ++ documentinfo.boxes
                        }
                  }
                , Cmd.none
                )

        GotText _ ->
            ( model, Cmd.none )

        GotData result ->
            case result of
                Ok documentinfo ->
                    ( { model
                        | documentinfo = documentinfo
                      }
                    , Cmd.none
                    )

                --Debug.log fullText (model, Cmd.none)
                Err _ ->
                    ( model, Cmd.none )

        Save ->
            ( model
            , Http.post
                { url = "/document?doc=" ++ model.document
                , expect = Http.expectString GotText
                , body = Http.jsonBody (encodeDocument model)
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

        SetModeEditing ->
            let
                editingstatus =
                    model.editingstatus
            in
            ( { model
                | editingstatus = { editingstatus | mode = Editing }
              }
            , Cmd.none
            )

        SetModeViewing ->
            let
                editingstatus =
                    model.editingstatus
            in
            ( { model
                | editingstatus = { editingstatus | mode = Viewing }
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ div []
            [ div []
                [ h1 [ style "display" "inline-block", style "margin" "0 0.5rem" ] [ text "Blackout" ]
                , h2 [ style "display" "inline-block", style "margin" "0 0.5rem" ] [ text model.documentinfo.name ]
                , br [] []
                , a
                    [ href ("/index.html?doc=" ++ model.documentinfo.prev)
                    , style "margin" "0.5em"
                    ]
                    [ text "Prev" ]
                , a
                    [ href "/menu.html"
                    , style "margin" "0.5em"
                    ]
                    [ text "Menu" ]
                , a
                    [ href ("/index.html?doc=" ++ model.documentinfo.next)
                    , style "margin" "0.5em"
                    ]
                    [ text "Next" ]
                , button
                    [ onClick
                        (if model.editingstatus.mode == Editing then
                            SetModeViewing

                         else
                            SetModeEditing
                        )
                    ]
                    [ text
                        (if model.editingstatus.mode == Editing then
                            "Done"

                         else
                            "Edit"
                        )
                    ]
                , button [ onClick Save ] [ text "Save" ]
                , button
                    [ if
                        model.editingstatus.mode
                            == Editing
                            && List.length model.editingstatus.newboxids
                            /= 0
                      then
                        onClick
                            UndoLastBox

                      else
                        style "display" "none"
                    ]
                    [ text "Undo" ]
                ]
            , div [ style "position" "relative" ]
                [ img
                    [ style "width" "100%"
                    , style "filter"
                        (if model.editingstatus.selecting then
                            "brightness(0.9)"

                         else
                            "unset"
                        )
                    , src model.documentinfo.url
                    ]
                    [ text "+" ]
                , blackBoxes model
                ]
            ]
        ]
    }


blackBoxes : Model -> Html Msg
blackBoxes model =
    div []
        -- A div to capture the non-box click events
        (div
            (List.append
                [ style "position" "absolute"
                , style "width" "100%"
                , style "height" "100%"
                , style "top" "0px"
                , style "left" "0px"
                ]
                (if model.editingstatus.mode == Editing then
                    [ style "cursor" "crosshair"
                    , onClickCreateBox
                    ]

                 else
                    []
                )
            )
            []
            :: List.map
                (\box ->
                    div
                        [ style "position" "absolute"
                        , style "backgroundColor"
                            (if box.shown then
                                "black"

                             else
                                "unset"
                            )
                        , style "top" (String.fromFloat (box.top - 0.5) ++ "%")
                        , style "left" (String.fromFloat (box.left - 0.5) ++ "%")
                        , style "width" (String.fromFloat (box.width + 1) ++ "%")
                        , style "height" (String.fromFloat (box.height + 1) ++ "%")
                        , style "cursor" "pointer"
                        , onClick (Toggle box.id)
                        ]
                        []
                )
                model.documentinfo.boxes
        )


genPosition : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Msg
genPosition sleft stop left top w h x y =
    ClickPosition
        { x = toFloat (x - left + sleft) / toFloat w
        , y = toFloat (y - top + stop) / toFloat h
        }


onClickCreateBox : Attribute Msg
onClickCreateBox =
    on "click"
        (Decode.map8 genPosition
            (Decode.at [ "target", "parentNode", "parentNode", "parentNode", "parentNode", "parentNode", "scrollLeft" ] Decode.int)
            (Decode.at [ "target", "parentNode", "parentNode", "parentNode", "parentNode", "parentNode", "scrollTop" ] Decode.int)
            (Decode.at [ "target", "parentNode", "parentNode", "offsetLeft" ] Decode.int)
            (Decode.at [ "target", "parentNode", "parentNode", "offsetTop" ] Decode.int)
            (Decode.at [ "target", "offsetWidth" ] Decode.int)
            (Decode.at [ "target", "offsetHeight" ] Decode.int)
            (Decode.at [ "clientX" ] Decode.int)
            (Decode.at [ "clientY" ] Decode.int)
        )
