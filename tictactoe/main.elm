module Main exposing (main)

import Browser
import Dict
import Html exposing (Html, button, div, h3, text, ul, li)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List
import List.Extra as LE


type Player
    = X
    | O
    | None

type alias GameState =
    { board : Board
    , currentPlayer : Player
    , turn : Int
    }
    
type alias Board =
    { squares : List Square }


type alias Square =
    { id : Int, value : Player }


type alias Model =
    { game : GameState
    , gameHistory : List GameState
    }


initialModel : Model
initialModel =
    { game = makeGame
    , gameHistory = []
    }

makeGame : GameState
makeGame =
    GameState emptyBoard X 0
    
makeBoard : List Square -> Board
makeBoard squares =
    Board squares
    

emptyBoard : Board
emptyBoard =
    { squares = List.map (\i -> Square i None) (List.range 0 8) }


type Msg
    = SquareClick Square
    | Reset
    | GoToTurn GameState


update : Msg -> Model -> Model
update msg model =
    let
        updateSquares squares square = 
            List.map 
                (\s ->
                    if s.id == square.id then
                        { id = square.id, value = model.game.currentPlayer }
                    else
                        s) squares
    in
    case msg of
        SquareClick square ->
            if square.value /= None then
                model

            else
                { model
                    | game = GameState (makeBoard <| updateSquares model.game.board.squares square) (switchPlayer model.game.currentPlayer) (model.game.turn + 1)
                    , gameHistory = model.game :: model.gameHistory
                }

        Reset ->
            { model | game = GameState emptyBoard X 0, gameHistory = [] }
            
        GoToTurn gameState ->
            { model | game = gameState, gameHistory = List.drop ((List.length model.gameHistory) - gameState.turn) (model.gameHistory) }  

view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ h3 [] [ text "TicTacToe" ]
        , div [] [text ("Turn " ++ String.fromInt model.game.turn) ]
        , viewBoard model.game.board
        , viewWinner model
        , button [ class "reset", onClick Reset ] [ text "Reset game" ]
        , viewGameHistory model.gameHistory
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    div
        [ class "board" ]
        (List.map viewBoardRow (LE.groupsOf 3 board.squares))


viewBoardRow : List Square -> Html Msg
viewBoardRow row =
    div
        [ class "board-row" ]
        (List.map viewSquare row)


viewSquare : Square -> Html Msg
viewSquare square =
    div [ class "board-square", onClick (SquareClick square) ] [ text <| playerToString square.value ]


viewWinner : Model -> Html msg
viewWinner model =
    let
        winner =
            calculateWinner model.game.board
    in
    if winner /= None then
        div
            [ class "winner" ]
            [ text ("The winner is " ++ playerToString winner) ]

    else
        div [] []
       
      
viewGameHistory : List GameState -> Html Msg
viewGameHistory gameHistory =
    let
        gameStateButton gameState =
            li 
                [] 
                [ button 
                    [ onClick (GoToTurn gameState) ] 
                    [ text <| "Go to turn " ++ (String.fromInt gameState.turn) ]
                ]
    in
        ul [class "gamestate-list"]
           (List.map gameStateButton <| List.reverse gameHistory)
        
        
        
playerToString : Player -> String
playerToString player =
    case player of
        X ->
            "X"

        O ->
            "O"

        None ->
            ""


switchPlayer : Player -> Player
switchPlayer player =
    case player of
        X ->
            O

        O ->
            X

        None ->
            None


calculateWinner : Board -> Player
calculateWinner board =
    let
        lines =
            [ ( 0, 1, 2 ), ( 3, 4, 5 ), ( 6, 7, 8 ), ( 0, 3, 6 ), ( 1, 4, 7 ), ( 2, 5, 8 ), ( 0, 4, 8 ), ( 2, 4, 6 ) ]


        -- easier lookup
        squareDict =
            Dict.fromList <| List.map (\s -> ( s.id, s )) <| board.squares

        checkWinner ( a, b, c ) =
            let
                getSquareValue id =
                    case Dict.get id squareDict of
                        Just square ->
                            square.value

                        _ ->
                            None

                firstCell =
                    getSquareValue a

                secondCell =
                    getSquareValue b

                thirdCell =
                    getSquareValue c
            in
            if firstCell == secondCell && firstCell == thirdCell then
                firstCell

            else
                None

        winner =
            let
                potentialWinner =
                    List.head <| List.filter (\s -> s /= None) <| List.map checkWinner lines
            in
            case potentialWinner of
                Just w ->
                    w

                _ ->
                    None
    in
    winner


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
