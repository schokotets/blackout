-- Menu to select different documents


module Menu exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Attribute, Html, button, div, h1, h2, img, li, map, text, ul)
import Html.Attributes exposing (src, style)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Document =
    { id : String
    , url : String
    , name : String
    }


type alias Model =
    { documents : List Document
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { documents = []
      }
    , Http.get
        { url = "/documentlist"
        , expect = Http.expectJson GotData documentListDecoder
        }
    )


documentListDecoder : Decode.Decoder (List Document)
documentListDecoder =
    Decode.field "documents" (Decode.list documentDecoder)


documentDecoder : Decode.Decoder Document
documentDecoder =
    Decode.map3 Document
        (Decode.field "id" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "name" Decode.string)



-- UPDATE


type Msg
    = Navigate String
    | GotData (Result Http.Error (List Document))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate doc ->
            ( model
            , Nav.load ("/index.html?doc=" ++ doc)
            )

        GotData result ->
            case result of
                Ok documents ->
                    ( { model
                        | documents = documents
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Blackout" ]
        , h2 [] [ text "Menü" ]
        , ul [] (documentList model.documents)
        ]


documentList : List Document -> List (Html Msg)
documentList documents =
    List.map
        (\document ->
            li
                [ style "cursor" "pointer"
                , style "margin" "1em 0"
                , onClick (Navigate document.id)
                ]
                [ text document.name ]
        )
        documents
