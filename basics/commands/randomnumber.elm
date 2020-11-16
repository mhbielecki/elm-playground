module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )


type Msg
    = Roll
    | NewNumber Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            -- The "magic" happens here. Send a command to the Elm runtime, saying that the Random.int generator should be
            -- run, and the value produced is sent back to the update function with the NewNumber message
            ( model, Random.generate NewNumber (Random.int 1 100) )

        NewNumber n ->
            ( n, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Roll ] [ text "Give me a random number between 1-100" ]
        , div [] [ text <| String.fromInt model ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
