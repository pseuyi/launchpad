port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, table, td, text, tr)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick)
import List exposing (..)


port connectMIDI : (String -> msg) -> Sub msg


port receiveMIDINoteOn : (Float -> msg) -> Sub msg


port receiveMIDINoteOff : (Float -> msg) -> Sub msg


port triggerAttack : Float -> Cmd msg


port triggerRelease : Float -> Cmd msg



-- TYPES


type alias Grid =
    { width : Int
    , height : Int
    }


type alias Note =
    Float



-- MAIN


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    { device : String, grid : Grid }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { device = "", grid = { height = 8, width = 8 } }, Cmd.none )



-- UPDATE


type Msg
    = ConnectDevice String
    | ReceiveMIDINoteOn Note
    | ReceiveMIDINoteOff Note


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConnectDevice name ->
            ( { model | device = name }
            , Cmd.none
            )

        ReceiveMIDINoteOn note ->
            ( model, triggerAttack note )

        ReceiveMIDINoteOff note ->
            ( model, triggerRelease note )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text
                (if model.device /= "" then
                    "currently connect to: " ++ model.device

                 else
                    "no connected devices"
                )
            ]
        , table []
            (generateGrid model.grid)
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ connectMIDI (\n -> ConnectDevice n)
        , receiveMIDINoteOn (\n -> ReceiveMIDINoteOn n)
        , receiveMIDINoteOff (\n -> ReceiveMIDINoteOff n)
        ]



-- HELPERS


generateGrid : Grid -> List (Html Msg)
generateGrid grid =
    map (\r -> tr [] (map (\d -> td [] [ input [ type_ "checkbox" ] [] ]) (List.repeat grid.width 0))) (List.repeat grid.height 0)
