module Main exposing (main)

import Browser
import Element exposing (Element, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, height, none, padding, paragraph, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Maybe exposing (andThen, map, withDefault)
import Maybe.Extra exposing (orElse, orElseLazy)



-- ZIPPER


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


type Crumb a
    = Root
    | Left a (Tree a) (Crumb a)
    | Right a (Tree a) (Crumb a)


type alias Zipper a =
    ( Tree a, Crumb a )


type Path
    = T
    | L Path
    | R Path


type alias Model =
    { counts : Zipper Int }


toZipper : Tree a -> Zipper a
toZipper tree =
    ( tree, Root )


fromZipper : Zipper a -> Tree a
fromZipper zipper =
    case reset zipper of
        ( tree, _ ) ->
            tree


goLeft : Zipper a -> Maybe (Zipper a)
goLeft ( tree, crumbs ) =
    case tree of
        Empty ->
            Nothing

        Node x l r ->
            Just ( l, Left x r crumbs )


goRight : Zipper a -> Maybe (Zipper a)
goRight ( tree, crumbs ) =
    case tree of
        Empty ->
            Nothing

        Node x l r ->
            Just ( r, Right x l crumbs )


goUp : Zipper a -> Maybe (Zipper a)
goUp ( tree, crumbs ) =
    case crumbs of
        Root ->
            Nothing

        Left x r c ->
            Just ( Node x tree r, c )

        Right x l c ->
            Just ( Node x l tree, c )


goRightSibling : Zipper a -> Maybe (Zipper a)
goRightSibling zipper =
    goUp zipper
        |> andThen goRight



{- TODO: Get goNext to work -}


goNext : Zipper a -> Maybe (Zipper a)
goNext zipper =
    let
        leftMost z =
            case goLeft z of
                Just left ->
                    leftMost left

                Nothing ->
                    Nothing
    in
    goUp zipper |> andThen goRight |> andThen leftMost


reset : Zipper a -> Zipper a
reset zipper =
    goUp zipper
        |> map reset
        |> withDefault zipper


getCurrent : Zipper a -> Tree a
getCurrent ( tree, _ ) =
    tree


getPath : Zipper a -> Path
getPath ( _, outerCrumbs ) =
    let
        getCrumbs crumbs =
            case crumbs of
                Root ->
                    T

                Left x r c ->
                    L (getCrumbs c)

                Right x l c ->
                    R (getCrumbs c)
    in
    getCrumbs outerCrumbs



-- MODEL


initialModel : Model
initialModel =
    { counts = ( Node 0 (Node 1 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 2 Empty (Node 3 Empty Empty)), Root ) }



-- UPDATE


type Msg
    = Increment
    | Decrement
    | GoLeft
    | GoRight
    | GoUp
    | GoNext
    | RightSibling


update : Msg -> Model -> Model
update msg model =
    let
        modify worker ( tree, crumbs ) =
            case tree of
                Empty ->
                    ( tree, crumbs )

                Node x l r ->
                    ( Node (worker x) l r, crumbs )

        navigate direction =
            case direction model.counts of
                Nothing ->
                    model

                Just zipper ->
                    { model | counts = zipper }
    in
    case msg of
        Increment ->
            { model
                | counts = modify (\x -> x + 1) model.counts
            }

        Decrement ->
            { model
                | counts = modify (\x -> x - 1) model.counts
            }

        GoLeft ->
            navigate goLeft

        GoRight ->
            navigate goRight

        GoUp ->
            navigate goUp

        GoNext ->
            navigate goNext

        RightSibling ->
            navigate goRightSibling



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout []
        (column [ Border.width 1, Border.color (rgb255 0 255 0) ]
            [ viewTree (getPath model.counts) T (fromZipper model.counts)
            , row [ Border.width 1, Border.color (rgb255 0 0 255), padding 5, spacing 5 ]
                [ Input.button buttonAtrs
                    { onPress = Just Increment, label = text "+1" }
                , Input.button
                    buttonAtrs
                    { onPress = Just Decrement, label = text "-1" }
                , Input.button
                    buttonAtrs
                    { onPress = Just GoLeft, label = text "L" }
                , Input.button
                    buttonAtrs
                    { onPress = Just GoRight, label = text "R" }
                , Input.button
                    buttonAtrs
                    { onPress = Just GoUp, label = text "U" }
                , Input.button
                    buttonAtrs
                    { onPress = Just GoNext, label = text "Next" }
                , Input.button
                    buttonAtrs
                    { onPress = Just RightSibling, label = text "Right sibling" }
                ]
            ]
        )


viewTree : Path -> Path -> Tree Int -> Element Msg
viewTree focusedPath path tree =
    let
        active =
            if focusedPath == path then
                Background.color purple

            else
                Background.color white
    in
    case tree of
        Empty ->
            el [ width <| px 70, height <| px 70, Font.center, alignTop, active ] <| text "()"

        Node x l r ->
            column [ Font.size 70, alignTop ] [ el [ centerX, active ] <| text <| String.fromInt x, row [] [ viewTree focusedPath (L path) l, viewTree focusedPath (R path) r ] ]



-- STYLING


blue =
    Element.rgb255 0 200 255


yellowIsh =
    Element.rgb255 0 204 204


purple =
    Element.rgb255 128 0 128


white =
    Element.rgb255 255 255 255


buttonAtrs =
    [ padding 5
    , Background.color blue
    , Element.focused
        [ Background.color purple ]
    ]



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
