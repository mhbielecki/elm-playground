module Main exposing (..)

import Element exposing (Element, column, minimum, maximum, el, text, row, alignRight, fill, width, rgb255, spacing, centerY, padding, height, px, htmlAttribute)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Events exposing (onMouseMove, onMouseDown, onMouseUp)
import Html.Attributes exposing (autocomplete, style)
import Browser
import Html exposing (Html)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder, field, string, int, float, at, map, map4, map6, map)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

view : Model -> Html Msg
view model = 
    Element.layout [ htmlAttribute <| (on "mouseup" (Decode.map MouseUp decoder))]
        (myRowOfStuff model)

        
type alias Model =
    { mouseDownInfo: MouseData
    , leftWidthInPx: Int
    , rightWidthInPx: Int
    , inDragMode: Bool }


initialModel : Model
initialModel =
    { mouseDownInfo = 
        { clientX = 0 }
    , leftWidthInPx = 200
    , rightWidthInPx = 800
    , inDragMode = False }


type Msg
    = MouseMove MouseData
    | MouseDown MouseData
    | MouseUp MouseData
    
type alias MouseData =
    { clientX : Int
    }
    
decoder : Decoder MouseData
decoder =
    map MouseData
        (at [ "clientX" ] int)


update : Msg -> Model -> Model
update msg model =
    case msg of           
        MouseMove data ->
            if not model.inDragMode then
                model
            else
                let 
                    delta = data.clientX - model.mouseDownInfo.clientX
                    
                    newLeftWidthInPx = model.leftWidthInPx + delta
                    
                    newRightWidthInPx = model.rightWidthInPx - delta
                in
                { model | leftWidthInPx = newLeftWidthInPx, rightWidthInPx = newRightWidthInPx, mouseDownInfo = data }
            
        MouseDown data ->
            { model | inDragMode = True, mouseDownInfo = data }
            
        MouseUp data ->
            let
                _ = Debug.log "mouseUp" data.clientX
            in
            { model | inDragMode = False }


  
myRowOfStuff model =
    let
        divider =
            el [ 
                 Html.Attributes.attribute "id" "filler-divider" |> htmlAttribute
                 , width <| px 10
                 , height fill
                 , htmlAttribute <| (on "mousedown" (Decode.map MouseDown decoder))
               ] (text "")
        
        mouseMoveEventAttr = htmlAttribute <| (on "mousemove" (Decode.map MouseMove decoder))
        
        attrList = [ width fill , height <| px 100 ] ++ (if model.inDragMode then [ mouseMoveEventAttr ] else [])
         
    in
    row attrList
        [ left model
        , divider
        , right model
        ]

left : Model -> Element Msg
left model =
        el
        [ Background.color (rgb255 240 0 245)
        , Font.color (rgb255 255 255 255)
        , height fill
        , width (px model.leftWidthInPx
            |> minimum 100
            |> maximum 800)
        ]
        (text "Left")
    
right : Model -> Element msg
right model =
        el
        [ Background.color (rgb255 123 5 45)
        , Font.color (rgb255 255 255 255)
        , height fill
        , width (fill
            |> minimum 100
        )
        ]
        (text "Right")
    