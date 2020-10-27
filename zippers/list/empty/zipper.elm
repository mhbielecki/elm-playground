module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { counts : Zipper Int}


type Zipper a =
    Zipper (List a) a (List a)

type ZipperWithHole a =
    ZipperWithHole (List a) (Maybe a) (List a)



updateCurrent : (a -> a) -> Zipper a -> Zipper a
updateCurrent worker (Zipper left current right) =
    Zipper left (worker current) right

next : Zipper a -> Zipper a
next ((Zipper left current right) as zipper) =
    case right of
      x::xs ->
          Zipper (current::left) x xs
      [] ->
          zipper

prev : Zipper a -> Zipper a
prev ((Zipper left current right) as zipper) =
    case left of
      x::xs ->
          Zipper xs x (current::right)
      [] ->
          zipper

toList : Zipper a -> List a
toList (Zipper left current right) =
   (List.reverse left) ++ current::right

fromList : a -> List a -> Zipper a
fromList x xs =
          Zipper [] x xs


updateCurrentWithHole : (a -> a) -> ZipperWithHole a -> ZipperWithHole a
updateCurrentWithHole worker (ZipperWithHole left maybeCurrent right) =
    ZipperWithHole left (Maybe.map worker maybeCurrent) right

nextWithHole : ZipperWithHole a -> ZipperWithHole a
nextWithHole ((ZipperWithHole left maybeCurrent right) as zipper) =
    let
        maybeAddCurrent xs =
            case maybeCurrent of
                Nothing ->
                    xs
                Just current ->
                    current::xs
    in
    case right of
      x::xs ->
          ZipperWithHole (maybeAddCurrent left) (Just x) xs
      [] ->
          zipper

prevWithHole : ZipperWithHole a -> ZipperWithHole a
prevWithHole ((ZipperWithHole left maybeCurrent right) as zipper) =
    let
        maybeAddCurrent xs =
            case maybeCurrent of
                Nothing ->
                    xs
                Just current ->
                    current::xs
    in
    case left of
      x::xs ->
          ZipperWithHole xs (Just x) (maybeAddCurrent right)
      [] ->
          zipper



toListWithHole : ZipperWithHole a -> List a
toListWithHole (ZipperWithHole left maybeCurrent right) =
    let
        maybeAddCurrent xs =
            case maybeCurrent of
                Nothing ->
                    xs
                Just current ->
                    current::xs
    in
   (List.reverse left) ++ maybeAddCurrent right

fromListWithHole : List a -> ZipperWithHole a
fromListWithHole list =
    case list of
        x::xs ->
          ZipperWithHole [] (Just x) xs
        [] ->
            ZipperWithHole [] Nothing []



initialModel : Model
initialModel =
    { counts = fromList 0 [0,0,0]}


type Msg
    = Increment
    | Decrement
    | Next
    | Prev


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | counts = updateCurrent (\i -> i + 1) model.counts }

        Decrement ->
            { model | counts = updateCurrent (\i -> i - 1) model.counts }
        Next ->
           { model | counts = next model.counts }
        Prev ->
            { model | counts = prev model.counts }

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+1" ]
        , button [ onClick Decrement ] [ text "-1" ]
        , button [ onClick Prev ] [ text "<" ]
        , button [ onClick Next ] [ text ">" ]
        , div [] <|
            List.map (\count -> text <| (String.fromInt count) ++ " ") (toList model.counts)
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
