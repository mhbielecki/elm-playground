module Main exposing (main)

import Browser
import Html exposing (Html, div)



-- Custom type


type Fish
    = Cod
    | Shark


makeFish : Fish
makeFish =
    Shark

-- pattern matching
whatFish: Fish -> String
whatFish fish =
    case fish of
        Cod -> "Cod is popular as a food with a mild flavour and a dense, flaky, white flesh."
        Shark -> "Sharks bite you!"



-- Custom type with associated data (Container type)


type A
    = B String
    | C Int
    | D Fish
    | E


makeAB : A
makeAB =
    B "hello"


makeAFish : A
makeAFish =
    D Cod
    
-- pattern matching
whatA: A -> String
whatA someA =
    case someA of
        B str -> "B with associated string " ++  str
        C n -> "C with associated int " ++ String.fromInt n
        D someFish -> "Fish " ++ whatFish someFish
        E -> "Simple E, nothing associated here!"


-- Custom type with type variables (they can be whatever)


type Car cool badass
    = Sport cool
    | Terrain badass


makeSportCar : Car String whatever
makeSportCar =
    Sport "Ferrari"  -- Sport "Ferrari" : Car String badass


makeTerrainCar : Car whatever String
makeTerrainCar =
    Terrain "Land Rover" -- Terrain ("Land Rover") : Car cool String


-- Pattern matching
whatCar: Car c b -> String
whatCar car =
    case car of
        Sport c -> "Wow, you are so cool with a sports car!"
        Terrain _ -> "You dont need roads"



type alias Model =
    Int


initialModel : Model
initialModel =
    0


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    div [] []


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }