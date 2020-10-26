module TodoList exposing (main)

import Browser
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias TodoItem =
    { id : Int
    , text : String
    }


type alias Model =
    { todoItems : List TodoItem
    , newItemContent : String
    , todoItemId : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { todoItems = []
      , newItemContent = ""
      , todoItemId = 1
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = AddNewItem String
    | UpdateCurrentItem String
    | DeleteItem Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddNewItem i ->
            ( { model | todoItems = TodoItem model.todoItemId i :: model.todoItems, newItemContent = "", todoItemId = model.todoItemId + 1 }, Cmd.none )

        UpdateCurrentItem content ->
            ( { model | newItemContent = content }, Cmd.none )

        DeleteItem id ->
            ( { model | todoItems = List.filter (\i -> i.id /= id) model.todoItems }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ placeholder "What to do?", value model.newItemContent, onInput UpdateCurrentItem, onSubmit (AddNewItem model.newItemContent) ] []
            , button [ onClick (AddNewItem model.newItemContent) ] [ text "Add item" ]
            ]
        , makeListView model.todoItems
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UTIL


makeListView : List TodoItem -> Html Msg
makeListView items =
    ul []
        (List.map (\i -> li [] [ text i.text, button [ class "delete-button", onClick (DeleteItem i.id) ] [ text "Done" ] ]) items)
